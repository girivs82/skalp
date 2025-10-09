//! Tests for physical constraint extraction in HIR builder

use skalp_frontend::hir::{PinLocation, SlewRate, Termination};
use skalp_frontend::parse_and_build_hir;

#[test]
fn test_extract_single_pin_constraint() {
    let source = r#"
        entity LedBlinker {
            in clk: clock @ {
                pin: "A1",
                io_standard: "LVCMOS33"
            }
        }
    "#;

    let hir = parse_and_build_hir(source).expect("Failed to parse");

    assert_eq!(hir.entities.len(), 1);
    let entity = &hir.entities[0];
    assert_eq!(entity.name, "LedBlinker");
    assert_eq!(entity.ports.len(), 1);

    let port = &entity.ports[0];
    assert_eq!(port.name, "clk");

    let constraints = port
        .physical_constraints
        .as_ref()
        .expect("No constraints found");

    match &constraints.pin_location {
        Some(PinLocation::Single(pin)) => {
            assert_eq!(pin, "A1");
        }
        _ => panic!("Expected single pin location"),
    }

    assert_eq!(constraints.io_standard.as_deref(), Some("LVCMOS33"));
}

#[test]
fn test_extract_multi_pin_constraint() {
    let source = r#"
        entity Counter {
            out count: nat[4] @ {
                pins: ["C1", "C2", "C3", "C4"],
                io_standard: "LVCMOS33"
            }
        }
    "#;

    let hir = parse_and_build_hir(source).expect("Failed to parse");

    assert_eq!(hir.entities.len(), 1);
    let entity = &hir.entities[0];
    let port = &entity.ports[0];

    let constraints = port
        .physical_constraints
        .as_ref()
        .expect("No constraints found");

    match &constraints.pin_location {
        Some(PinLocation::Multiple(pins)) => {
            assert_eq!(pins.len(), 4);
            assert_eq!(pins, &vec!["C1", "C2", "C3", "C4"]);
        }
        _ => panic!("Expected multiple pin location"),
    }

    assert_eq!(constraints.io_standard.as_deref(), Some("LVCMOS33"));
}

#[test]
fn test_extract_slew_and_termination() {
    let source = r#"
        entity SerialPort {
            out tx: bit @ {
                pin: "B2",
                slew: fast,
                pull: up
            }
        }
    "#;

    let hir = parse_and_build_hir(source).expect("Failed to parse");

    let entity = &hir.entities[0];
    let port = &entity.ports[0];

    let constraints = port
        .physical_constraints
        .as_ref()
        .expect("No constraints found");

    assert_eq!(constraints.slew_rate, Some(SlewRate::Fast));
    assert_eq!(constraints.termination, Some(Termination::PullUp));

    match &constraints.pin_location {
        Some(PinLocation::Single(pin)) => {
            assert_eq!(pin, "B2");
        }
        _ => panic!("Expected single pin location"),
    }
}

#[test]
fn test_extract_boolean_constraints() {
    let source = r#"
        entity Input {
            in data: bit @ {
                pin: "D1",
                schmitt: true,
                diff_term: false
            }
        }
    "#;

    let hir = parse_and_build_hir(source).expect("Failed to parse");

    let entity = &hir.entities[0];
    let port = &entity.ports[0];

    let constraints = port
        .physical_constraints
        .as_ref()
        .expect("No constraints found");

    assert_eq!(constraints.schmitt_trigger, Some(true));
    assert_eq!(constraints.diff_term, Some(false));
}

#[test]
fn test_extract_bank_constraint() {
    let source = r#"
        entity BankTest {
            in sig: bit @ {
                pin: "A1",
                bank: 0
            }
        }
    "#;

    let hir = parse_and_build_hir(source).expect("Failed to parse");

    let entity = &hir.entities[0];
    let port = &entity.ports[0];

    let constraints = port
        .physical_constraints
        .as_ref()
        .expect("No constraints found");

    assert_eq!(constraints.bank, Some(0));
}

#[test]
fn test_port_without_constraints() {
    let source = r#"
        entity NoConstraints {
            in clk: clock
            out led: bit
        }
    "#;

    let hir = parse_and_build_hir(source).expect("Failed to parse");

    let entity = &hir.entities[0];
    assert_eq!(entity.ports.len(), 2);

    for port in &entity.ports {
        assert!(
            port.physical_constraints.is_none(),
            "Port {} should not have constraints",
            port.name
        );
    }
}
