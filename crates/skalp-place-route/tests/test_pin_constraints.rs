//! Tests for pin constraint application in placement
//!
//! Verifies that the placer correctly applies physical constraints from LIR netlists.

use skalp_frontend::hir::{DriveStrength, PhysicalConstraints, PinLocation, SlewRate, Termination};
use skalp_lir::netlist::{Module, Netlist, Port, PortDirection};
use skalp_place_route::device::Device;
use skalp_place_route::placer::{Placer, PlacerConfig};

/// Create a test netlist with pin constraints
fn create_test_netlist_with_constraints() -> Netlist {
    let mut netlist = Netlist::new("test_top".to_string());

    let mut module_ports = Vec::new();

    // Add port with single pin constraint
    let clk_port = Port {
        name: "clk".to_string(),
        direction: PortDirection::Input,
        width: 1,
        physical_constraints: Some(PhysicalConstraints {
            pin_location: Some(PinLocation::Single("A1".to_string())),
            io_standard: Some("LVCMOS33".to_string()),
            drive_strength: Some(DriveStrength::Ma8),
            slew_rate: Some(SlewRate::Fast),
            termination: Some(Termination::PullUp),
            schmitt_trigger: None,
            bank: None,
            diff_term: None,
        }),
    };
    module_ports.push(clk_port);

    // Add port with multi-pin constraint (bus)
    let data_port = Port {
        name: "data".to_string(),
        direction: PortDirection::Input,
        width: 4,
        physical_constraints: Some(PhysicalConstraints {
            pin_location: Some(PinLocation::Multiple(vec![
                "B1".to_string(),
                "B2".to_string(),
                "C1".to_string(),
                "C2".to_string(),
            ])),
            io_standard: Some("LVCMOS33".to_string()),
            drive_strength: Some(DriveStrength::Ma12),
            slew_rate: Some(SlewRate::Slow),
            termination: Some(Termination::PullDown),
            schmitt_trigger: None,
            bank: None,
            diff_term: None,
        }),
    };
    module_ports.push(data_port);

    // Add port with differential pair constraint
    let diff_port = Port {
        name: "diff_clk".to_string(),
        direction: PortDirection::Input,
        width: 1,
        physical_constraints: Some(PhysicalConstraints {
            pin_location: Some(PinLocation::Differential {
                positive: "D1".to_string(),
                negative: "D2".to_string(),
            }),
            io_standard: Some("LVDS".to_string()),
            drive_strength: None,
            slew_rate: None,
            termination: None,
            schmitt_trigger: None,
            bank: None,
            diff_term: Some(true),
        }),
    };
    module_ports.push(diff_port);

    let module = Module {
        name: "test_module".to_string(),
        ports: module_ports,
        instances: Vec::new(),
        nets: Vec::new(),
    };

    netlist.modules.push(module);
    netlist
}

#[test]
fn test_single_pin_constraint_application() {
    let device = Device::ice40_hx1k();
    let config = PlacerConfig::default();
    let mut placer = Placer::new(config, device);

    let netlist = create_test_netlist_with_constraints();

    // Apply pin constraints
    let result = placer.apply_pin_constraints(&netlist);
    assert!(
        result.is_ok(),
        "Pin constraint application failed: {:?}",
        result.err()
    );

    // Verify that clk pin was assigned to A1's position
    assert!(placer.fixed_placements().contains_key("clk"));

    // Verify I/O configuration was stored
    assert!(placer.io_configurations().contains_key("clk"));
    let clk_config = &placer.io_configurations()["clk"];
    assert_eq!(clk_config.location, "A1");
    assert_eq!(clk_config.io_standard, Some("LVCMOS33".to_string()));
    assert_eq!(clk_config.drive_strength, Some(8));
}

#[test]
fn test_multi_pin_bus_constraint_application() {
    let device = Device::ice40_hx1k();
    let config = PlacerConfig::default();
    let mut placer = Placer::new(config, device);

    let netlist = create_test_netlist_with_constraints();

    // Apply pin constraints
    let result = placer.apply_pin_constraints(&netlist);
    assert!(
        result.is_ok(),
        "Pin constraint application failed: {:?}",
        result.err()
    );

    // Verify that each bit of the data bus was assigned
    assert!(placer.fixed_placements().contains_key("data[0]"));
    assert!(placer.fixed_placements().contains_key("data[1]"));
    assert!(placer.fixed_placements().contains_key("data[2]"));
    assert!(placer.fixed_placements().contains_key("data[3]"));

    // Verify I/O configurations
    assert!(placer.io_configurations().contains_key("data[0]"));
    let data0_config = &placer.io_configurations()["data[0]"];
    assert_eq!(data0_config.location, "B1");
    assert_eq!(data0_config.drive_strength, Some(12));
}

#[test]
fn test_differential_pair_constraint_application() {
    let device = Device::ice40_hx1k();
    let config = PlacerConfig::default();
    let mut placer = Placer::new(config, device);

    let netlist = create_test_netlist_with_constraints();

    // Apply pin constraints
    let result = placer.apply_pin_constraints(&netlist);
    assert!(
        result.is_ok(),
        "Pin constraint application failed: {:?}",
        result.err()
    );

    // Verify that differential pair was assigned
    assert!(placer.fixed_placements().contains_key("diff_clk_p"));
    assert!(placer.fixed_placements().contains_key("diff_clk_n"));

    // Verify I/O configurations
    assert!(placer.io_configurations().contains_key("diff_clk_p"));
    assert!(placer.io_configurations().contains_key("diff_clk_n"));

    let diff_p_config = &placer.io_configurations()["diff_clk_p"];
    assert_eq!(diff_p_config.location, "D1");
    assert_eq!(diff_p_config.io_standard, Some("LVDS".to_string()));

    let diff_n_config = &placer.io_configurations()["diff_clk_n"];
    assert_eq!(diff_n_config.location, "D2");
}

#[test]
fn test_invalid_pin_detection() {
    let device = Device::ice40_hx1k();
    let config = PlacerConfig::default();
    let mut placer = Placer::new(config, device);

    // Create netlist with invalid pin
    let mut netlist = Netlist::new("test".to_string());
    let port = Port {
        name: "invalid".to_string(),
        direction: PortDirection::Input,
        width: 1,
        physical_constraints: Some(PhysicalConstraints {
            pin_location: Some(PinLocation::Single("Z99".to_string())), // Invalid pin
            io_standard: None,
            drive_strength: None,
            slew_rate: None,
            termination: None,
            schmitt_trigger: None,
            bank: None,
            diff_term: None,
        }),
    };
    let module = Module {
        name: "test".to_string(),
        ports: vec![port],
        instances: Vec::new(),
        nets: Vec::new(),
    };
    netlist.modules.push(module);

    // Apply constraints - should fail
    let result = placer.apply_pin_constraints(&netlist);
    assert!(result.is_err(), "Should reject invalid pin");
}

#[test]
fn test_constraint_count() {
    let device = Device::ice40_hx1k();
    let config = PlacerConfig::default();
    let mut placer = Placer::new(config, device);

    let netlist = create_test_netlist_with_constraints();

    let result = placer.apply_pin_constraints(&netlist);
    assert!(result.is_ok());

    // Count total constrained pins:
    // - clk: 1 pin
    // - data[0..3]: 4 pins
    // - diff_clk_p, diff_clk_n: 2 pins
    // Total: 7 pins
    assert_eq!(placer.fixed_placements().len(), 7);
    assert_eq!(placer.io_configurations().len(), 7);
}

#[test]
fn test_no_constraints() {
    let device = Device::ice40_hx1k();
    let config = PlacerConfig::default();
    let mut placer = Placer::new(config, device);

    // Create netlist without constraints
    let mut netlist = Netlist::new("test".to_string());
    let port = Port {
        name: "unconstrained".to_string(),
        direction: PortDirection::Input,
        width: 1,
        physical_constraints: None,
    };
    let module = Module {
        name: "test".to_string(),
        ports: vec![port],
        instances: Vec::new(),
        nets: Vec::new(),
    };
    netlist.modules.push(module);

    let result = placer.apply_pin_constraints(&netlist);
    assert!(result.is_ok());

    // No constraints should be applied
    assert_eq!(placer.fixed_placements().len(), 0);
    assert_eq!(placer.io_configurations().len(), 0);
}
