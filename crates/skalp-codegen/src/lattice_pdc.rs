//! Lattice Radiant physical constraints (`.pdc`) from a design's port
//! constraints, so the SystemVerilog a build writes for a Lattice device can
//! be placed by the vendor's tools with the same pins and I/O settings.

use skalp_frontend::hir::{
    DriveStrength, PhysicalConstraints, PinLocation, SlewRate, Termination, VrefSetting,
};
use skalp_mir::{DataType, Mir, Module, PortDirection};
use std::fmt::Write;

/// The modules no other module instantiates: the design's top levels
fn top_modules(mir: &Mir) -> impl Iterator<Item = &Module> {
    mir.modules.iter().filter(|m| {
        !mir.modules
            .iter()
            .any(|p| p.instances.iter().any(|i| i.module == m.id))
    })
}

/// The `ldc_set_port -iobuf` settings of one port
fn iobuf_settings(c: &PhysicalConstraints, direction: &PortDirection) -> Vec<String> {
    let mut s = Vec::new();
    if let Some(std) = &c.io_standard {
        s.push(format!("IO_TYPE={std}"));
    }
    if let Some(d) = &c.drive_strength {
        let ma = match d {
            DriveStrength::Ma4 => 4,
            DriveStrength::Ma8 => 8,
            DriveStrength::Ma12 => 12,
            DriveStrength::Ma16 => 16,
        };
        s.push(format!("DRIVE={ma}"));
    }
    if let Some(r) = &c.slew_rate {
        s.push(
            match r {
                SlewRate::Fast => "SLEWRATE=FAST",
                SlewRate::Slow => "SLEWRATE=SLOW",
                SlewRate::Medium => "SLEWRATE=MED",
            }
            .to_string(),
        );
    }
    if let Some(t) = &c.termination {
        s.push(
            match t {
                Termination::None => "PULLMODE=NONE",
                Termination::PullUp => "PULLMODE=UP",
                Termination::PullDown => "PULLMODE=DOWN",
                Termination::Keeper => "PULLMODE=KEEPER",
            }
            .to_string(),
        );
    }
    // an input compares against the bank's internal reference
    if c.vref_level.is_some() && !matches!(direction, PortDirection::Output) {
        s.push("VREF=VREF_INT".to_string());
    }
    if let Some(ohms) = c.odt_ohms {
        s.push(format!("TERMINATION={ohms}"));
    }
    s
}

/// A period in ns, without trailing zeros
fn format_ns(ns: f64) -> String {
    let s = format!("{ns:.4}");
    s.trim_end_matches('0').trim_end_matches('.').to_string()
}

/// What the reference level of a port asks for, for a comment: Radiant takes
/// the level from the bank's reference generator, not from the port
fn vref_note(v: &VrefSetting) -> String {
    match v {
        VrefSetting::SupplyHundredthsPercent(h) => {
            format!("{}.{:02}% of the bank supply", h / 100, h % 100)
        }
        VrefSetting::Millivolts(mv) => format!("{mv} mV"),
        VrefSetting::DeviceCode(c) => format!("generator code {c}"),
    }
}

/// The registers marked as the first stage of a synchronizer (`#[cdc]`), by
/// hierarchical name below `prefix` (`phy/dll_lock_m`)
fn synchronizers(mir: &Mir, module: &Module, prefix: &str, out: &mut Vec<String>) {
    if module.vendor_ip_config.is_some() {
        return;
    }
    for signal in module.signals.iter().filter(|s| s.cdc_config.is_some()) {
        out.push(format!("{prefix}{}", signal.name));
    }
    for instance in &module.instances {
        if let Some(child) = mir.modules.iter().find(|m| m.id == instance.module) {
            synchronizers(mir, child, &format!("{prefix}{}/", instance.name), out);
        }
    }
}

/// The `.pdc` of the design: the top-level ports' pins and I/O settings, a
/// clock on each clock pin whose frequency `pin_mhz` knows (the vendor's
/// tools derive the PLL and divided clocks from it), and no timing into the
/// first register of each synchronizer (`#[cdc]`), whose input changes with
/// no relation to its clock. `None` when there is nothing to constrain.
pub fn generate_lattice_pdc(mir: &Mir, pin_mhz: &dyn Fn(&str) -> Option<f64>) -> Option<String> {
    let mut pdc = String::new();
    let mut any = false;
    for top in top_modules(mir) {
        for port in &top.ports {
            if let (DataType::Clock { .. }, Some(PinLocation::Single(pin))) = (
                &port.port_type,
                port.physical_constraints
                    .as_ref()
                    .and_then(|c| c.pin_location.as_ref()),
            ) {
                if let Some(mhz) = pin_mhz(pin) {
                    let _ = writeln!(
                        pdc,
                        "create_clock -name {{{0}}} -period {1} [get_ports {0}]",
                        port.name,
                        format_ns(1000.0 / mhz)
                    );
                    any = true;
                }
            }
        }
        let mut syncs = Vec::new();
        synchronizers(mir, top, "", &mut syncs);
        for sync in syncs {
            let _ = writeln!(pdc, "set_false_path -to [get_cells {{{sync}*}}]");
            any = true;
        }
        for port in &top.ports {
            let Some(c) = &port.physical_constraints else {
                continue;
            };
            // the SystemVerilog names of the port's pins, with their sites
            let pins: Vec<(String, Option<&str>)> = match &c.pin_location {
                Some(PinLocation::Single(p)) => vec![(port.name.clone(), Some(p.as_str()))],
                Some(PinLocation::Multiple(ps)) => ps
                    .iter()
                    .enumerate()
                    .map(|(i, p)| (format!("{}[{i}]", port.name), Some(p.as_str())))
                    .collect(),
                // the vendor places a differential pair's complement itself
                Some(PinLocation::Differential { positive, .. }) => {
                    vec![(port.name.clone(), Some(positive.as_str()))]
                }
                None => vec![(port.name.clone(), None)],
            };
            let settings = iobuf_settings(c, &port.direction);
            if let Some(v) = &c.vref_level {
                let _ = writeln!(
                    pdc,
                    "# {}: reference level {} (set by the bank's reference generator)",
                    port.name,
                    vref_note(v)
                );
            }
            for (name, site) in pins {
                let target = if name.contains('[') {
                    format!("{{{name}}}")
                } else {
                    name
                };
                if let Some(site) = site {
                    let _ = writeln!(
                        pdc,
                        "ldc_set_location -site {{{site}}} [get_ports {target}]"
                    );
                    any = true;
                }
                if !settings.is_empty() {
                    let _ = writeln!(
                        pdc,
                        "ldc_set_port -iobuf {{{}}} [get_ports {target}]",
                        settings.join(" ")
                    );
                    any = true;
                }
            }
        }
    }
    any.then(|| format!("# Physical constraints generated by SKALP\n{pdc}"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_frontend::parse_and_build_hir;
    use skalp_mir::{MirCompiler, OptimizationLevel};

    #[test]
    fn pins_and_io_settings_of_the_top_ports() {
        let source = r#"
entity Top {
    in clk: clock @ { pin: "P24", io_standard: "LVCMOS33" }
    out led: bit[2] @ { pins: ["R5", "R4"], io_standard: "LVCMOS33" }
    inout dq: bit @ { pin: "AA6", io_standard: "LVSTL_I", odt: "60ohm", vref_code: 49 }
    out cs: bit @ { pin: "AC19", io_standard: "LVSTL_I" }
    in d: bit
    out y: bit
}

entity Sync {
    in clk: clock
    in a: bit
    out y: bit
}

impl Sync {
    #[cdc]
    signal meta: bit = 0
    signal held: bit = 0
    on(clk.rise) {
        meta = a
        held = meta
    }
    y = held
}

impl Top {
    signal r: bit[2] = 0
    inst s = Sync {
        clk: clk,
        a: d
    }
    on(clk.rise) {
        r = r + 1
    }
    led = r
    cs = r[0]
    dq = r[1]
    y = s.y
}
"#;
        let hir = parse_and_build_hir(source).expect("parse");
        let mir = MirCompiler::new()
            .with_optimization_level(OptimizationLevel::None)
            .compile_to_mir(&hir)
            .expect("mir");
        let pdc = generate_lattice_pdc(&mir, &|pin| (pin == "P24").then_some(125.0))
            .expect("constraints");
        for line in [
            "ldc_set_location -site {P24} [get_ports clk]",
            "ldc_set_port -iobuf {IO_TYPE=LVCMOS33} [get_ports clk]",
            "ldc_set_location -site {R5} [get_ports {led[0]}]",
            "ldc_set_location -site {R4} [get_ports {led[1]}]",
            "ldc_set_location -site {AA6} [get_ports dq]",
            "ldc_set_port -iobuf {IO_TYPE=LVSTL_I VREF=VREF_INT TERMINATION=60} [get_ports dq]",
            "ldc_set_port -iobuf {IO_TYPE=LVSTL_I} [get_ports cs]",
            "# dq: reference level generator code 49",
            "create_clock -name {clk} -period 8 [get_ports clk]",
            "set_false_path -to [get_cells {s/meta*}]",
        ] {
            assert!(pdc.contains(line), "missing `{line}` in:\n{pdc}");
        }
    }
}
