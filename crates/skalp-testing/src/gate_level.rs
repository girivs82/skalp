//! Gate-level simulation of chosen entities inside a behavioural testbench.
//!
//! Each instance of a chosen entity is synthesized for the device, as a build
//! for that device would synthesize it, and the netlist replaces the entity's
//! body: its own logic runs as the gates synthesis made, while the vendor
//! primitives it instantiates (black boxes in the netlist) run their
//! simulation models, and the rest of the testbench (stimulus, memory
//! models) stays behavioural. This is how a design built from vendor
//! primitives is checked at gate level, as vendor flows check a netlist
//! against the vendor's simulation libraries.

use anyhow::{bail, Context, Result};
use skalp_lir::gate_netlist::GateNetlist;
use skalp_lir::netlist_to_skalp::{netlist_to_skalp, EntityPort};
use skalp_lir::synth::SynthPreset;
use skalp_mir::mir::{DataType, Mir, Module, ModuleId, PortDirection};
use skalp_mir::MirCompiler;
use std::collections::HashMap;
use std::path::Path;

/// A pass over each synthesized netlist before it is simulated (a
/// place-and-route step, say)
pub type NetlistPass<'a> = &'a (dyn Fn(GateNetlist) -> GateNetlist + Sync);

/// Replace the body of every instantiated module of `mir` that is one of
/// `entities` (or a specialization of one) with its synthesized netlist for
/// `device`. Returns the names of the modules replaced; it is an error when
/// none is.
pub fn substitute_gate_level(
    mir: &mut Mir,
    entities: &[&str],
    device: &str,
    preset: SynthPreset,
    pass: Option<NetlistPass<'_>>,
) -> Result<Vec<String>> {
    let family = skalp_frontend::primitive_family(device)
        .with_context(|| format!("no device family for '{device}'"))?;
    let library = skalp_lir::get_stdlib_library(family)
        .map_err(|e| anyhow::anyhow!("technology library '{family}': {e:?}"))?;

    let instantiated: std::collections::HashSet<ModuleId> = mir
        .modules
        .iter()
        .flat_map(|m| m.instances.iter().map(|i| i.module))
        .collect();
    let targets: Vec<String> = mir
        .modules
        .iter()
        .filter(|m| {
            instantiated.contains(&m.id)
                && entities
                    .iter()
                    .any(|e| m.name == *e || m.specialization_of.as_deref() == Some(*e))
        })
        .map(|m| m.name.clone())
        .collect();
    if targets.is_empty() {
        bail!(
            "no instance of {} in the design to simulate at gate level",
            entities.join(", ")
        );
    }

    for name in &targets {
        let netlist = synthesize_module(mir, name, &library, preset)?;
        let netlist = match pass {
            Some(pass) => pass(netlist),
            None => netlist,
        };
        let module = mir
            .modules
            .iter()
            .find(|m| &m.name == name)
            .expect("target module");
        let ports = entity_ports(module)?;
        let entity = format!("GateLevel_{name}");
        let generics_of = |prim: &str| primitive_generics(mir, prim);
        let source = netlist_to_skalp(&netlist, &entity, &ports, &generics_of)
            .map_err(|e| anyhow::anyhow!("gate-level netlist of {name}: {e}"))?;
        if let Ok(dir) = std::env::var("SKALP_GATE_LEVEL_DUMP") {
            let _ = std::fs::write(Path::new(&dir).join(format!("{entity}.sk")), &source);
        }
        let gates = compile_netlist(&source, &entity, device)
            .with_context(|| format!("gate-level netlist of {name}"))?;
        splice(mir, name, gates, &entity);
    }
    Ok(targets)
}

/// The netlist of module `name` and the modules under it, as a build for
/// the device would synthesize it
fn synthesize_module(
    mir: &Mir,
    name: &str,
    library: &skalp_lir::tech_library::TechLibrary,
    preset: SynthPreset,
) -> Result<GateNetlist> {
    let mut sub = mir.clone();
    sub.retain_reachable_from(name);
    let (hier, has_async) =
        skalp_lir::lower_mir_hierarchical_for_optimize_first_with_top(&sub, Some(name));
    if has_async {
        bail!("{name} has async (NCL) logic, which gate-level substitution does not cover");
    }
    let step_errors = skalp_lir::check_step_conditions(&hier);
    if !step_errors.is_empty() {
        bail!("{}", step_errors.join("\n"));
    }
    Ok(skalp_lir::synthesize_hierarchical(&hier, library, preset).flatten_for_library(library))
}

/// The ports of a module as the netlist writer takes them
fn entity_ports(module: &Module) -> Result<Vec<EntityPort>> {
    module
        .ports
        .iter()
        .map(|p| {
            let clock = matches!(p.port_type, DataType::Clock { .. });
            match &p.port_type {
                DataType::Struct(_)
                | DataType::Enum(_)
                | DataType::Union(_)
                | DataType::Array(..) => bail!(
                    "port '{}' of {} is not a bit vector, which gate-level substitution \
                     does not cover",
                    p.name,
                    module.name
                ),
                _ => {}
            }
            if p.direction == PortDirection::InOut {
                bail!(
                    "inout port '{}' of {}: gate-level substitution does not cover inout ports",
                    p.name,
                    module.name
                );
            }
            Ok(EntityPort {
                name: p.name.clone(),
                width: skalp_lir::data_type_width(&p.port_type) as usize,
                output: p.direction == PortDirection::Output,
                clock,
            })
        })
        .collect()
}

/// The generic parameters of vendor primitive `prim`, if the design has it
/// (the generics its specializations were given values for)
fn primitive_generics(mir: &Mir, prim: &str) -> Option<Vec<String>> {
    let mut generics: Option<Vec<String>> = None;
    for config in mir
        .modules
        .iter()
        .filter_map(|m| m.vendor_ip_config.as_ref())
        .filter(|c| c.ip_name == prim)
    {
        let known = generics.get_or_insert_with(Vec::new);
        for g in &config.generics {
            if !known.contains(g) {
                known.push(g.clone());
            }
        }
    }
    generics
}

/// The MIR of the netlist entity and the primitives it instantiates
fn compile_netlist(source: &str, entity: &str, device: &str) -> Result<Mir> {
    let context = skalp_frontend::parse_and_build_compilation_context_from_source_for_device(
        Path::new(&format!("{entity}.sk")),
        source.to_string(),
        Some(device),
    )?;
    let mut mir = MirCompiler::new()
        .compile_to_mir_with_modules(&context.main_hir, &context.module_hirs)
        .map_err(|e| anyhow::anyhow!("{e}"))?;
    if !mir.retain_reachable_from(entity) {
        bail!("netlist entity {entity} not compiled");
    }
    Ok(mir)
}

/// Put the netlist entity of `gates` in place of module `name` of `mir`,
/// adding the modules it instantiates that `mir` does not have (by name)
fn splice(mir: &mut Mir, name: &str, gates: Mir, entity: &str) {
    let mut next = mir.modules.iter().map(|m| m.id.0).max().unwrap_or(0) + 1;
    let by_name: HashMap<String, ModuleId> =
        mir.modules.iter().map(|m| (m.name.clone(), m.id)).collect();
    let target = by_name[name];
    let mut remap: HashMap<ModuleId, ModuleId> = HashMap::new();
    let mut added = Vec::new();
    let mut netlist_module = None;
    for module in gates.modules {
        if module.name == entity {
            remap.insert(module.id, target);
            netlist_module = Some(module);
        } else if let Some(&id) = by_name.get(&module.name) {
            remap.insert(module.id, id);
        } else {
            let id = ModuleId(next);
            next += 1;
            remap.insert(module.id, id);
            added.push(module);
        }
    }
    let fix = |mut m: Module, remap: &HashMap<ModuleId, ModuleId>| {
        m.id = remap[&m.id];
        for inst in &mut m.instances {
            inst.module = remap[&inst.module];
        }
        m
    };
    let mut netlist_module = fix(netlist_module.expect("netlist entity"), &remap);
    let slot = mir
        .modules
        .iter_mut()
        .find(|m| m.id == target)
        .expect("target module");
    netlist_module.name = slot.name.clone();
    netlist_module.specialization_of = slot.specialization_of.clone();
    netlist_module.is_from_main_source = slot.is_from_main_source;
    *slot = netlist_module;
    for m in added {
        mir.modules.push(fix(m, &remap));
    }
}
