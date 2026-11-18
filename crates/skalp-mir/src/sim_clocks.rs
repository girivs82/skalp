//! Simulation clocks of primitive models.
//!
//! A vendor primitive (`#[lattice_ip(...)]` and the like) is a black box to
//! synthesis, but its entity can carry a behavioural model for simulation.
//! Models of clock-generating or delay blocks (a PLL, a delay line, a DQS
//! buffer) need a clock finer than any of the design's own, which the real
//! primitive does not have as a port. Such a model declares it in its
//! attribute (`sim_clock = "tick"`) and the design never connects it.
//!
//! For simulation, [`wire_sim_clocks`] connects every one of those ports to a
//! top-level clock input named [`SIM_TICK`], adding a `sim_tick` input to each
//! module on the way down that does not have one. Synthesis never runs this,
//! and leaves the ports out of the primitive's cell.

use crate::mir::{
    DataType, Expression, ExpressionKind, LValue, Mir, ModuleId, Port, PortDirection, PortId,
};
use std::collections::HashMap;

/// Name of the top-level clock input that drives every primitive model's
/// simulation clock
pub const SIM_TICK: &str = "sim_tick";

/// Connect the simulation clocks of every primitive model under `top` to a
/// top-level `sim_tick` input. Returns whether `top` needed one (and now has
/// it). Modules reached from `top` that contain such a model get a `sim_tick`
/// input of their own, connected from their parent.
pub fn wire_sim_clocks(mir: &mut Mir, top: ModuleId) -> bool {
    let mut needs: HashMap<ModuleId, bool> = HashMap::new();
    if !needs_tick(mir, top, &mut needs, &mut Vec::new()) {
        return false;
    }
    let mut next_port = mir
        .modules
        .iter()
        .flat_map(|m| m.ports.iter().map(|p| p.id.0))
        .max()
        .map_or(0, |m| m + 1);
    // Every module that needs a tick and is not a primitive model gets a
    // `sim_tick` input and passes it on to each instance that needs one
    let wired: Vec<ModuleId> = needs
        .iter()
        .filter(|(_, n)| **n)
        .map(|(id, _)| *id)
        .collect();
    for id in wired {
        let Some(idx) = mir.modules.iter().position(|m| m.id == id) else {
            continue;
        };
        if is_model(mir, id) {
            continue;
        }
        let tick = match mir.modules[idx].ports.iter().find(|p| p.name == SIM_TICK) {
            Some(p) => p.id,
            None => {
                let port = PortId(next_port);
                next_port += 1;
                mir.modules[idx].ports.push(Port {
                    id: port,
                    name: SIM_TICK.to_string(),
                    direction: PortDirection::Input,
                    port_type: DataType::Clock { domain: None },
                    clock_domain_name: None,
                    physical_constraints: None,
                    span: None,
                    detection_config: None,
                });
                port
            }
        };
        let tick_ref = Expression::with_unknown_type(ExpressionKind::Ref(LValue::Port(tick)));
        let child_ports: Vec<Vec<String>> = mir.modules[idx]
            .instances
            .iter()
            .map(|inst| {
                if !needs.get(&inst.module).copied().unwrap_or(false) {
                    return Vec::new();
                }
                match mir.modules.iter().find(|m| m.id == inst.module) {
                    Some(child) => match &child.vendor_ip_config {
                        Some(cfg) if !cfg.sim_clocks.is_empty() => cfg.sim_clocks.clone(),
                        _ => vec![SIM_TICK.to_string()],
                    },
                    None => Vec::new(),
                }
            })
            .collect();
        for (inst, ports) in mir.modules[idx].instances.iter_mut().zip(child_ports) {
            for port in ports {
                inst.connections
                    .entry(port)
                    .or_insert_with(|| tick_ref.clone());
            }
        }
    }
    true
}

fn is_model(mir: &Mir, id: ModuleId) -> bool {
    mir.modules
        .iter()
        .find(|m| m.id == id)
        .and_then(|m| m.vendor_ip_config.as_ref())
        .is_some_and(|cfg| !cfg.sim_clocks.is_empty())
}

/// Whether `id` is a primitive model with a simulation clock, or contains one
fn needs_tick(
    mir: &Mir,
    id: ModuleId,
    memo: &mut HashMap<ModuleId, bool>,
    stack: &mut Vec<ModuleId>,
) -> bool {
    if let Some(&n) = memo.get(&id) {
        return n;
    }
    if stack.contains(&id) {
        return false;
    }
    let Some(module) = mir.modules.iter().find(|m| m.id == id) else {
        return false;
    };
    let n = if is_model(mir, id) {
        true
    } else {
        stack.push(id);
        let children: Vec<ModuleId> = module.instances.iter().map(|i| i.module).collect();
        let mut any = false;
        for child in children {
            any |= needs_tick(mir, child, memo, stack);
        }
        stack.pop();
        any
    };
    memo.insert(id, n);
    n
}
