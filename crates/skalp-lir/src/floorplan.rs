//! Floorplan regions from the source (`constraint physical { floorplan {
//! region "name" { area: (x0%, y0%, x1%, y1%), instances: [...] } } }`),
//! resolved against the elaborated hierarchy and kept with the gate netlist
//! so place and route (including a later `skalp pnr` of the saved netlist)
//! sees them.

use crate::gate_netlist::{Cell, GateNetlist};
use crate::mir_to_lir::HierarchicalMirToLirResult;
use serde::{Deserialize, Serialize};
use skalp_frontend::hir::{GlobalConstraint, Hir};
use skalp_mir::Mir;

/// A placement region of the floorplan and what goes in it
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FloorplanRegion {
    /// The region's name, as the source gives it
    pub name: String,
    /// The rectangle `(x0, y0, x1, y1)` in percent of the die's width and
    /// height, corners included
    pub area: (f64, f64, f64, f64),
    /// What the region holds
    pub members: Vec<FloorplanMember>,
}

/// Something a floorplan region holds
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum FloorplanMember {
    /// Every cell of an instance (by instance path, `top.core`)
    Instance(String),
    /// The cells built for one signal or port of an instance: its
    /// registers and latches, and any cell driving a net of its name
    Signal {
        /// The instance path, such as `top`
        instance: String,
        /// The name the instance's own cells carry after its path (its
        /// entity's, as elaborated): `<instance>.<entity>.<signal>...`
        entity: String,
        /// The signal's name in its entity
        signal: String,
    },
}

impl FloorplanMember {
    /// Whether `cell` of `netlist` belongs to this member
    pub fn holds(&self, netlist: &GateNetlist, cell: &Cell) -> bool {
        match self {
            FloorplanMember::Instance(path) => cell
                .path
                .strip_prefix(path.as_str())
                .is_some_and(|rest| rest.starts_with('.')),
            FloorplanMember::Signal {
                instance,
                entity,
                signal,
            } => {
                let own = cell
                    .path
                    .strip_prefix(instance.as_str())
                    .and_then(|rest| rest.strip_prefix('.'))
                    .and_then(|rest| rest.strip_prefix(entity.as_str()))
                    .and_then(|rest| rest.strip_prefix('.'))
                    .is_some_and(|rest| names_signal(rest, signal));
                own || cell.outputs.iter().any(|&net| {
                    netlist.nets.get(net.0 as usize).is_some_and(|net| {
                        net.name
                            .strip_prefix(instance.as_str())
                            .and_then(|rest| rest.strip_prefix('.'))
                            .is_some_and(|rest| names_signal(rest, signal))
                    })
                })
            }
        }
    }

    /// How the member reads in a message
    pub fn describe(&self) -> String {
        match self {
            FloorplanMember::Instance(path) => format!("instance {path}"),
            FloorplanMember::Signal {
                instance, signal, ..
            } => format!("signal {instance}.{signal}"),
        }
    }
}

/// Whether `text` is `signal`, one of its bits (`signal[3]`) or something
/// below it (`signal.lat0`)
fn names_signal(text: &str, signal: &str) -> bool {
    text.strip_prefix(signal)
        .is_some_and(|rest| rest.is_empty() || rest.starts_with('.') || rest.starts_with('['))
}

/// The floorplan regions `hir` declares, with each name it places resolved
/// in the elaborated hierarchy: a name (`counter`) or hierarchical path
/// (`core.alu`) below the top names an instance, or a signal or port of
/// the instance above it. A name that is neither is an error.
pub fn resolve_floorplan(
    hir: &Hir,
    mir: &Mir,
    hierarchy: &HierarchicalMirToLirResult,
) -> Result<Vec<FloorplanRegion>, String> {
    let mut regions = Vec::new();
    for constraint in &hir.global_constraints {
        let GlobalConstraint::Floorplan(floorplan) = constraint else {
            continue;
        };
        for region in &floorplan.regions {
            let members = region
                .instances
                .iter()
                .map(|name| resolve_name(mir, hierarchy, name))
                .collect::<Result<Vec<_>, _>>()
                .map_err(|e| format!("floorplan region \"{}\": {e}", region.name))?;
            regions.push(FloorplanRegion {
                name: region.name.clone(),
                area: region.area,
                members,
            });
        }
    }
    Ok(regions)
}

fn resolve_name(
    mir: &Mir,
    hierarchy: &HierarchicalMirToLirResult,
    name: &str,
) -> Result<FloorplanMember, String> {
    let path = format!("top.{name}");
    if hierarchy.instances.contains_key(&path) {
        return Ok(FloorplanMember::Instance(path));
    }
    let (parent, signal) = match name.rsplit_once('.') {
        Some((parent, signal)) => (format!("top.{parent}"), signal),
        None => ("top".to_string(), name),
    };
    let Some(instance) = hierarchy.instances.get(&parent) else {
        return Err(format!("`{name}`: there is no instance {parent}"));
    };
    let module = mir
        .modules
        .iter()
        .find(|m| m.name == instance.module_name)
        .ok_or_else(|| format!("`{name}`: no module {}", instance.module_name))?;
    let declared = module.signals.iter().any(|s| s.name == signal)
        || module.ports.iter().any(|p| p.name == signal);
    if !declared {
        return Err(format!(
            "`{name}` is neither an instance nor a signal of {}",
            instance.module_name
        ));
    }
    Ok(FloorplanMember::Signal {
        instance: parent,
        entity: instance.lir_result.lir.name.clone(),
        signal: signal.to_string(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gate_netlist::{CellId, GateNet, GateNetId};

    fn cell(netlist: &mut GateNetlist, path: &str, output: &str) -> Cell {
        let net = netlist.add_net(GateNet::new(GateNetId(0), output.to_string()));
        let mut cell = Cell::new_comb(
            CellId(0),
            "LUT4".to_string(),
            "lib".to_string(),
            0.0,
            path.to_string(),
            vec![],
            vec![net],
        );
        let id = netlist.add_cell(cell.clone());
        cell.id = id;
        cell
    }

    #[test]
    fn a_member_holds_its_own_cells_only() {
        let mut netlist = GateNetlist::new("t".into(), "lib".into());
        let latch = cell(&mut netlist, "top.Probe.ring_3.lat0", "top._t1");
        let bit = cell(&mut netlist, "top.aig_12", "top.count[4]");
        let other = cell(&mut netlist, "top.Probe.ring_30.lat0", "top._t2");
        let child = cell(&mut netlist, "top.core.Alu.acc", "top.core.acc[0]");
        let ring = FloorplanMember::Signal {
            instance: "top".into(),
            entity: "Probe".into(),
            signal: "ring_3".into(),
        };
        assert!(ring.holds(&netlist, &latch));
        assert!(!ring.holds(&netlist, &other), "ring_30 is not ring_3");
        let count = FloorplanMember::Signal {
            instance: "top".into(),
            entity: "Probe".into(),
            signal: "count".into(),
        };
        assert!(count.holds(&netlist, &bit));
        assert!(!count.holds(&netlist, &latch));
        let core = FloorplanMember::Instance("top.core".into());
        assert!(core.holds(&netlist, &child));
        assert!(!core.holds(&netlist, &latch));
        assert!(!FloorplanMember::Instance("top.co".into()).holds(&netlist, &child));
        // A child instance's cells are not the parent's signal of that name
        let alu = FloorplanMember::Signal {
            instance: "top".into(),
            entity: "Probe".into(),
            signal: "Alu".into(),
        };
        assert!(!alu.holds(&netlist, &child));
    }
}
