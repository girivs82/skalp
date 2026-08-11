//! FPGA leg of the power-domain model (spec power-domain subset).
//!
//! Commodity FPGA fabric has no user-partitionable power islands: VCCINT is
//! one grid. What the silicon does give you is per-bank I/O rails (VCCIO),
//! and what prototyping flows need is honest STUBBING of ASIC power intent.
//! This module implements both:
//!
//! - `check_bank_io_compatibility`: a port's `io_standard` must match its
//!   bank's declared rail voltage (`constraint physical { bank N { voltage:
//!   3.3, ... } }`). An LVCMOS33 pin on a 1.8 V bank is a build error.
//! - `fpga_power_posture`: `switched(...)`/`regulated(...)` domains are
//!   unimplementable in fabric. Targeting an FPGA with such domains is an
//!   error unless stubbing is requested, in which case every stubbed
//!   element is REPORTED (never silent): switches become always-on,
//!   regulated rails are assumed externally supplied, states collapse to
//!   `on`. The dependent-failure reality is also stated: same-fabric logic
//!   shares VCCINT and is never supply-independent.

use skalp_frontend::hir::{GlobalConstraint, Hir, HirPowerDerivation};

/// Nominal voltage (mV) required by a known I/O standard. Unknown standards
/// return None and are skipped (no false errors on exotic standards).
fn io_standard_voltage_mv(std_name: &str) -> Option<u32> {
    let norm = std_name.to_uppercase().replace(['_', '-'], "");
    match norm.as_str() {
        "LVTTL" | "LVTTL33" | "LVCMOS33" => Some(3300),
        "LVCMOS25" | "LVDS25" | "SSTL2" | "SSTL2I" | "SSTL2II" => Some(2500),
        "LVCMOS18" | "SSTL18" | "SSTL18I" | "SSTL18II" | "HSTL18" => Some(1800),
        "LVCMOS15" | "HSTL15" => Some(1500),
        "LVCMOS12" | "SSTL12" => Some(1200),
        _ => None,
    }
}

/// Parse a declared bank voltage: `3.3`, `3.3V`, `1800mV`.
fn parse_voltage_mv(text: &str) -> Option<u32> {
    let t = text.trim();
    if let Some(v) = t.strip_suffix("mV") {
        return v.trim().parse::<f64>().ok().map(|v| v as u32);
    }
    let v = t.strip_suffix('V').unwrap_or(t);
    v.trim()
        .parse::<f64>()
        .ok()
        .map(|v| (v * 1000.0).round() as u32)
}

/// Check every port's `io_standard` against its bank's declared rail.
/// Returns build-failing error strings; empty when no bank declares a
/// voltage or no port names a bank.
pub fn check_bank_io_compatibility(hir: &Hir) -> Vec<String> {
    let mut errors = Vec::new();

    // A domain's rail voltage: its `on` state, else its single stated voltage.
    let domain_voltage = |name: &str| -> Option<u32> {
        let d = hir.power_domain_decls.iter().find(|d| d.name == name)?;
        d.states
            .iter()
            .find(|s| s.name == "on")
            .and_then(|s| s.voltage_mv)
            .or_else(|| {
                let volts: Vec<u32> = d.states.iter().filter_map(|s| s.voltage_mv).collect();
                if volts.len() == 1 {
                    Some(volts[0])
                } else {
                    None
                }
            })
    };

    let banks: Vec<(u32, Option<u32>)> = hir
        .global_constraints
        .iter()
        .filter_map(|c| match c {
            GlobalConstraint::Bank(b) => {
                let lit = b.voltage.as_deref().and_then(parse_voltage_mv);
                let dom = b.domain.as_deref().and_then(|name| {
                    if !hir.power_domain_decls.iter().any(|d| d.name == name) {
                        errors.push(format!(
                            "bank {}: `domain: {}` references an undeclared power domain",
                            b.bank_id, name
                        ));
                        return None;
                    }
                    let v = domain_voltage(name);
                    if v.is_none() {
                        errors.push(format!(
                            "bank {}: power domain `{}` declares no usable rail voltage (add an `on` state with a voltage)",
                            b.bank_id, name
                        ));
                    }
                    v
                });
                // A literal voltage and a domain-derived one must not disagree.
                if let (Some(l), Some(d)) = (lit, dom) {
                    if l != d {
                        errors.push(format!(
                            "bank {}: literal voltage {:.1} V disagrees with domain `{}`'s rail {:.1} V — remove one or make them match",
                            b.bank_id,
                            l as f64 / 1000.0,
                            b.domain.as_deref().unwrap_or("?"),
                            d as f64 / 1000.0
                        ));
                    }
                }
                Some((b.bank_id, dom.or(lit)))
            }
            _ => None,
        })
        .collect();
    if banks.is_empty() {
        return errors;
    }
    let bank_voltage = |id: u32| -> Option<u32> {
        banks
            .iter()
            .find(|(bid, _)| *bid == id)
            .and_then(|(_, v)| *v)
    };

    for entity in &hir.entities {
        for port in &entity.ports {
            let Some(pc) = &port.physical_constraints else {
                continue;
            };
            let (Some(bank), Some(io_std)) = (pc.bank, pc.io_standard.as_deref()) else {
                continue;
            };
            let (Some(rail_mv), Some(io_mv)) = (bank_voltage(bank), io_standard_voltage_mv(io_std))
            else {
                continue;
            };
            if rail_mv != io_mv {
                errors.push(format!(
                    "port `{}` of `{}`: io_standard {} requires a {:.1} V rail, but bank {} declares {:.1} V (VCCIO mismatch)",
                    port.name,
                    entity.name,
                    io_std,
                    io_mv as f64 / 1000.0,
                    bank,
                    rail_mv as f64 / 1000.0
                ));
            }
        }
    }
    errors
}

/// Decide how declared power domains map onto an FPGA target.
///
/// - No declarations, or external-only: Ok(None) — rails are the board's
///   business, nothing to stub.
/// - `switched`/`regulated` present, `allow_stub` false: Err with the list
///   and the remedy.
/// - `allow_stub` true: Ok(Some(report)) enumerating every stubbed element.
pub fn fpga_power_posture(hir: &Hir, allow_stub: bool) -> Result<Option<String>, String> {
    let non_external: Vec<(&str, &str)> = hir
        .power_domain_decls
        .iter()
        .filter_map(|d| match &d.derivation {
            HirPowerDerivation::External => None,
            HirPowerDerivation::Regulated { .. } => Some((d.name.as_str(), "regulated")),
            HirPowerDerivation::Switched { .. } => Some((d.name.as_str(), "switched")),
        })
        .collect();

    if non_external.is_empty() {
        return Ok(None);
    }

    if !allow_stub {
        let list = non_external
            .iter()
            .map(|(n, k)| format!("`{}` ({})", n, k))
            .collect::<Vec<_>>()
            .join(", ");
        return Err(format!(
            "design declares power domains that FPGA fabric cannot implement: {}. \
             Fabric has no user power islands — switches and on-die regulators do not exist there. \
             For ASIC prototyping, re-run with --power-stub to treat them as always-on \
             (every stubbed element is reported).",
            list
        ));
    }

    let mut report = String::new();
    report.push_str("⚡ FPGA power-stub report (ASIC power intent prototyped as always-on):\n");
    for d in &hir.power_domain_decls {
        match &d.derivation {
            HirPowerDerivation::External => {}
            HirPowerDerivation::Switched { parent, .. } => {
                report.push_str(&format!(
                    "   `{}`: power switch from `{}` STUBBED — always-on; no isolation clamps, no retention loss, off/retention states unreachable\n",
                    d.name, parent
                ));
            }
            HirPowerDerivation::Regulated { parent, macro_name } => {
                report.push_str(&format!(
                    "   `{}`: regulator{} on `{}` NOT instantiated — rail assumed externally supplied at its `on` voltage\n",
                    d.name,
                    macro_name
                        .as_deref()
                        .map(|m| format!(" `{}`", m))
                        .unwrap_or_default(),
                    parent
                ));
            }
        }
    }
    // The blanket caveat is true but unactionable. Name the mechanisms whose
    // declared independence the design is actually relying on: those are the
    // FMEDA claims that do not survive the move to fabric.
    let collapsed = vccint_collapsed_mechanisms(hir);
    if collapsed.is_empty() {
        report.push_str(
            "   NOTE: all fabric logic shares VCCINT. No #[safety_mechanism] in this design \
             relies on supply independence, so nothing is invalidated by that.\n",
        );
    } else {
        report.push_str(
            "   NOTE: all fabric logic shares VCCINT. These mechanisms are declared \
             supply-independent from what they monitor, and are NOT on this device — their \
             dependent-failure claim does not hold here:\n",
        );
        for (mech, mech_domain, ctx, ctx_domain) in collapsed {
            report.push_str(&format!(
                "     `{}` (in `{}`): `{}` vs `{}` — both are fabric logic on VCCINT\n",
                mech, ctx, mech_domain, ctx_domain
            ));
        }
    }
    Ok(Some(report))
}

/// `#[safety_mechanism]` entities whose declared supply independence collapses
/// on FPGA fabric: their domain and their instantiating context's domain have
/// disjoint ancestry in the source, but both are fabric logic sharing VCCINT.
///
/// Returns (mechanism, mechanism domain, context entity, context domain).
fn vccint_collapsed_mechanisms(hir: &Hir) -> Vec<(String, String, String, String)> {
    use std::collections::{HashMap, HashSet};

    let parent_of: HashMap<&str, Option<&str>> = hir
        .power_domain_decls
        .iter()
        .map(|d| {
            let parent = match &d.derivation {
                HirPowerDerivation::External => None,
                HirPowerDerivation::Regulated { parent, .. }
                | HirPowerDerivation::Switched { parent, .. } => Some(parent.as_str()),
            };
            (d.name.as_str(), parent)
        })
        .collect();
    let ancestors = |name: &str| -> HashSet<String> {
        let mut set = HashSet::new();
        let mut cur = Some(name);
        while let Some(n) = cur {
            if !set.insert(n.to_string()) {
                break;
            }
            cur = parent_of.get(n).copied().flatten();
        }
        set
    };

    let instantiated: HashSet<_> = hir
        .implementations
        .iter()
        .flat_map(|i| i.instances.iter().map(|inst| inst.entity))
        .collect();
    let mut out = Vec::new();
    let mut stack: Vec<(skalp_frontend::hir::EntityId, Option<String>, String, usize)> = hir
        .entities
        .iter()
        .filter(|e| !instantiated.contains(&e.id))
        .map(|e| {
            (
                e.id,
                e.power_domain_config
                    .as_ref()
                    .map(|c| c.domain_name.clone()),
                e.name.clone(),
                0usize,
            )
        })
        .collect();

    while let Some((eid, eff, name, depth)) = stack.pop() {
        if depth > 64 {
            continue;
        }
        let Some(imp) = hir.implementations.iter().find(|i| i.entity == eid) else {
            continue;
        };
        for inst in &imp.instances {
            let Some(child) = hir.entities.iter().find(|e| e.id == inst.entity) else {
                continue;
            };
            let child_eff = child
                .power_domain_config
                .as_ref()
                .map(|c| c.domain_name.clone())
                .or_else(|| eff.clone());

            if child.safety_mechanism_config.is_some() {
                if let (Some(md), Some(cd)) = (&child_eff, &eff) {
                    // Independent in the source is exactly the claim fabric breaks.
                    if md != cd && ancestors(md).is_disjoint(&ancestors(cd)) {
                        out.push((child.name.clone(), md.clone(), name.clone(), cd.clone()));
                    }
                }
            }
            stack.push((child.id, child_eff, child.name.clone(), depth + 1));
        }
    }
    out
}

/// Ordered (source-domain, sink-domain) pairs for every instance edge that
/// crosses a power-domain boundary, taken from the HIR containment tree.
///
/// Level shifting and isolation are properties of a CROSSING — which domains
/// actually exchange nets — not of the supply tree. Two rails that are
/// siblings under one source still need shifters between them if their
/// operating voltages differ, and a parent/child supply pair at the same
/// voltage needs none.
pub fn crossing_domain_pairs(hir: &Hir) -> Vec<(String, String)> {
    use std::collections::HashSet;
    let mut pairs: Vec<(String, String)> = Vec::new();
    let mut seen: HashSet<(String, String)> = HashSet::new();
    if hir.power_domain_decls.is_empty() {
        return pairs;
    }
    let instantiated: HashSet<_> = hir
        .implementations
        .iter()
        .flat_map(|i| i.instances.iter().map(|inst| inst.entity))
        .collect();
    let mut stack: Vec<(skalp_frontend::hir::EntityId, Option<String>, usize)> = hir
        .entities
        .iter()
        .filter(|e| !instantiated.contains(&e.id))
        .map(|e| {
            (
                e.id,
                e.power_domain_config
                    .as_ref()
                    .map(|c| c.domain_name.clone()),
                0usize,
            )
        })
        .collect();
    while let Some((eid, eff, depth)) = stack.pop() {
        if depth > 64 {
            continue;
        }
        let Some(imp) = hir.implementations.iter().find(|i| i.entity == eid) else {
            continue;
        };
        for inst in &imp.instances {
            let Some(child) = hir.entities.iter().find(|e| e.id == inst.entity) else {
                continue;
            };
            let child_eff = child
                .power_domain_config
                .as_ref()
                .map(|c| c.domain_name.clone())
                .or_else(|| eff.clone());
            if let (Some(p), Some(c)) = (&eff, &child_eff) {
                if p != c && seen.insert((p.clone(), c.clone())) {
                    pairs.push((p.clone(), c.clone()));
                }
            }
            stack.push((child.id, child_eff, depth + 1));
        }
    }
    pairs
}

/// (domain, instance-path prefix) pairs from the HIR containment tree, for
/// attributing flattened netlist elements to power domains by instance-path
/// prefix — matched against a cell's hierarchical path (primary; survives
/// port stitching) or a net name's provenance prefix (`wd.cnt[0]`,
/// fallback). A bound root entity gets prefix "".
pub fn domain_instance_prefixes(hir: &Hir) -> Vec<(String, String)> {
    use std::collections::HashSet;
    let mut prefixes: Vec<(String, String)> = Vec::new();
    if hir.power_domain_decls.is_empty() {
        return prefixes;
    }
    let instantiated: HashSet<_> = hir
        .implementations
        .iter()
        .flat_map(|i| i.instances.iter().map(|inst| inst.entity))
        .collect();
    let mut stack: Vec<(skalp_frontend::hir::EntityId, String)> = hir
        .entities
        .iter()
        .filter(|e| !instantiated.contains(&e.id))
        .map(|e| (e.id, String::new()))
        .collect();
    for (eid, prefix) in &stack {
        if let Some(entity) = hir.entities.iter().find(|e| e.id == *eid) {
            if let Some(cfg) = &entity.power_domain_config {
                prefixes.push((cfg.domain_name.clone(), prefix.clone()));
            }
        }
    }
    while let Some((eid, prefix)) = stack.pop() {
        if prefix.matches('.').count() > 64 {
            continue;
        }
        let Some(imp) = hir.implementations.iter().find(|i| i.entity == eid) else {
            continue;
        };
        for inst in &imp.instances {
            let Some(child) = hir.entities.iter().find(|e| e.id == inst.entity) else {
                continue;
            };
            let child_prefix = if prefix.is_empty() {
                inst.name.clone()
            } else {
                format!("{}.{}", prefix, inst.name)
            };
            if let Some(cfg) = &child.power_domain_config {
                prefixes.push((cfg.domain_name.clone(), child_prefix.clone()));
            }
            stack.push((child.id, child_prefix));
        }
    }
    prefixes
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn voltage_parsing() {
        assert_eq!(parse_voltage_mv("3.3"), Some(3300));
        assert_eq!(parse_voltage_mv("3.3V"), Some(3300));
        assert_eq!(parse_voltage_mv("1800mV"), Some(1800));
        assert_eq!(parse_voltage_mv("1.8"), Some(1800));
    }

    #[test]
    fn io_standard_table() {
        assert_eq!(io_standard_voltage_mv("LVCMOS33"), Some(3300));
        assert_eq!(io_standard_voltage_mv("LVDS_25"), Some(2500));
        assert_eq!(io_standard_voltage_mv("lvcmos18"), Some(1800));
        assert_eq!(io_standard_voltage_mv("EXOTIC_IO"), None);
    }
}
