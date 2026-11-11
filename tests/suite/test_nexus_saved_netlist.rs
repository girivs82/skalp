//! `skalp synth -d nexus-...` saves design_gates.json for `skalp pnr` to place
//! and route later. That file must already hold the IO cells the pin
//! constraints ask for, including the on-chip oscillator behind `pin: "OSC"`:
//! `skalp pnr` reads only the netlist, so a netlist saved before the pads
//! were added came back with an undriven clock and no LED pad.

use skalp_lir::GateNetlist;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn saved_nexus_netlist_keeps_the_oscillator_and_pads() {
    let dir = TempDir::new().unwrap();
    let source = format!(
        "{}/tests/fixtures/nexus_blinker.sk",
        env!("CARGO_MANIFEST_DIR")
    );
    let output = Command::new(env!("CARGO_BIN_EXE_skalp"))
        .args([
            "synth",
            &source,
            "-d",
            "nexus-lfcpnx100",
            "--optimize",
            "quick",
            "-o",
        ])
        .arg(dir.path())
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "synth failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let json = std::fs::read_to_string(dir.path().join("design_gates.json")).unwrap();
    let netlist: GateNetlist = serde_json::from_str(&json).unwrap();
    let at = |pin: &str| {
        netlist
            .cells
            .iter()
            .find(|c| c.parameters.get("LOC").map(String::as_str) == Some(pin))
    };

    let osc = at("OSC").expect("the saved netlist has no oscillator cell");
    let clock = osc.outputs[0];
    assert!(
        netlist
            .cells
            .iter()
            .any(|c| c.clock == Some(clock) || c.inputs.contains(&clock)),
        "the oscillator drives nothing"
    );
    assert!(at("R8").is_some(), "the saved netlist has no LED pad");
}
