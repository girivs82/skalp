//! Constants across instance boundaries.
//!
//! Each instance of a hierarchical design is synthesized on its own, so a
//! constant computed in one instance did not reach the logic of another: an
//! adder instance given `carry_in: 0`, or two operands the parent computes
//! from generics, still built a full carry chain, and its constant sum still
//! reached the parent as an unknown. [`propagate`] finds, to a fixed point,
//! the signals of each instance that hold a constant because of what its
//! parent or its children compute, and [`tie_readers`] makes the logic that
//! reads them use the constant, so synthesis folds it.
//!
//! Only facts that hold whatever the design's inputs and registers do are
//! used, starting from the constants written in the design, so the result is
//! the same circuit.

use crate::lir::{Lir, LirOp, LirSignalId};
use crate::mir_to_lir::{HierarchicalMirToLirResult, PortConnectionInfo};
use crate::synth::lir_to_aig::constant_signal_values;
use indexmap::IndexMap;
use std::collections::{BTreeMap, BTreeSet};

/// The signals of one instance (by name) that hold a constant, with its value
pub type Ties = BTreeMap<String, u64>;

/// Most rounds of propagation: each round carries constants one instance
/// boundary further, so this bounds the depth a constant crosses
const MAX_ROUNDS: usize = 64;

fn mask(width: u32) -> u64 {
    if width >= 64 {
        u64::MAX
    } else {
        (1u64 << width) - 1
    }
}

fn signal<'a>(lir: &'a Lir, name: &str) -> Option<(LirSignalId, &'a crate::lir::LirSignal)> {
    lir.get_signal(name).map(|s| (s.id, s))
}

/// The signals each instance can treat as constants. `lirs` holds the LIR
/// synthesis will use for each instance path taking part: compiled IP,
/// black boxes and asynchronous (NCL) instances are left out, and so are
/// their connections.
pub fn propagate(
    hier: &HierarchicalMirToLirResult,
    lirs: &IndexMap<String, &Lir>,
) -> IndexMap<String, Ties> {
    let mut ties: IndexMap<String, Ties> = lirs.keys().map(|p| (p.clone(), Ties::new())).collect();
    // Constant values of each instance's signals that others read: its
    // outputs, and the signals tied in it
    let mut values: IndexMap<String, Ties> = IndexMap::new();
    let mut dirty: BTreeSet<String> = lirs.keys().cloned().collect();
    // Each instance's own input ports, before synthesis made the signals
    // its children drive inputs too
    let own_ports: IndexMap<&str, BTreeSet<&str>> = lirs
        .keys()
        .map(|path| {
            let lir = &hier.instances[path].lir_result.lir;
            let ports = lir
                .inputs
                .iter()
                .map(|s| lir.signals[s.0 as usize].name.as_str())
                .collect();
            (path.as_str(), ports)
        })
        .collect();

    for _ in 0..MAX_ROUNDS {
        if dirty.is_empty() {
            break;
        }
        for path in std::mem::take(&mut dirty) {
            let mut lir = lirs[&path].clone();
            tie_readers(&mut lir, &ties[&path]);
            let outputs: Vec<LirSignalId> = lir.outputs.clone();
            let mut known = ties[&path].clone();
            for (sig, value) in outputs.iter().zip(constant_signal_values(&lir, &outputs)) {
                if let Some(value) = value {
                    known.insert(lir.signals[sig.0 as usize].name.clone(), value);
                }
            }
            values.insert(path, known);
        }

        for (path, inst) in &hier.instances {
            let Some(child) = lirs.get(path) else {
                continue;
            };
            let Some(parent_path) = path.rfind('.').map(|dot| &path[..dot]) else {
                continue;
            };
            let Some(parent) = lirs.get(parent_path) else {
                continue;
            };
            let parent_values = &values[parent_path];
            let child_values = &values[path.as_str()];
            for (port, conn) in &inst.port_connections {
                let Some((port_id, port_signal)) = signal(child, port) else {
                    continue;
                };
                if child.inputs.contains(&port_id) && !child.clocks.contains(&port_id) {
                    // Into the instance: what the parent connects is constant
                    let value = match conn {
                        PortConnectionInfo::Constant(v) => Some(*v),
                        PortConnectionInfo::Signal(name) => parent_values.get(name).copied(),
                        PortConnectionInfo::InstancePort(inst, port) => {
                            parent_values.get(&format!("{inst}_{port}")).copied()
                        }
                        PortConnectionInfo::Range(name, _, low) => parent_values
                            .get(name)
                            .filter(|_| *low < 64)
                            .map(|v| v >> low),
                        PortConnectionInfo::BitSelect(name, bit) => parent_values
                            .get(name)
                            .filter(|_| *bit < 64)
                            .map(|v| v >> bit),
                    };
                    let width = port_signal.width;
                    if let Some(value) = value.filter(|_| width <= 64) {
                        let value = value & mask(width);
                        if ties[path.as_str()].get(port) != Some(&value) {
                            ties[path.as_str()].insert(port.clone(), value);
                            dirty.insert(path.clone());
                        }
                    }
                } else if child.outputs.contains(&port_id) {
                    // Out of the instance: the parent signal it drives
                    let (PortConnectionInfo::Signal(name), Some(&value)) =
                        (conn, child_values.get(port))
                    else {
                        continue;
                    };
                    let Some((parent_id, parent_signal)) = signal(parent, name) else {
                        continue;
                    };
                    // Only a signal this instance drives, which the parent
                    // reads as an input: not one of the parent's own ports
                    // (driven from above), nor one its logic drives
                    if !parent.inputs.contains(&parent_id)
                        || parent.clocks.contains(&parent_id)
                        || parent_signal.width != port_signal.width
                        || own_ports[parent_path].contains(name.as_str())
                    {
                        continue;
                    }
                    if ties[parent_path].get(name) != Some(&value) {
                        ties[parent_path].insert(name.clone(), value);
                        dirty.insert(parent_path.to_string());
                    }
                }
            }
        }
    }
    ties.retain(|_, t| !t.is_empty());
    ties
}

/// Make the logic of `lir` that reads each signal of `ties` read its constant
/// instead. The signal itself stays (a port keeps its connection).
pub fn tie_readers(lir: &mut Lir, ties: &Ties) {
    for (name, &value) in ties {
        let Some((sig, s)) = signal(lir, name) else {
            continue;
        };
        if lir.clocks.contains(&sig) {
            continue;
        }
        let width = s.width;
        let constant = lir.add_signal(format!("{name}__tied"), width);
        lir.add_node(
            LirOp::Constant {
                width,
                value: value & mask(width),
            },
            Vec::new(),
            constant,
            format!("{name}__tied"),
        );
        let last = lir.nodes.len() - 1;
        for node in &mut lir.nodes[..last] {
            for input in &mut node.inputs {
                if *input == sig {
                    *input = constant;
                }
            }
        }
    }
}
