//! Level-sensitive latches from combinational self-reference.
//!
//! A continuous assignment that keeps its own value on some path,
//!
//! ```text
//! l = if g { d } else { l }
//! l = if a { x } else if b { y } else { l }
//! ```
//!
//! is a transparent latch: `l` follows the chosen data while any arm other
//! than the self-reference is selected, and holds otherwise. Lowered as is,
//! the self-reference is a combinational loop, which logic synthesis cannot
//! order and used to break at an arbitrary point (silently changing what the
//! design does). This pass recognises the pattern on the word-level IR and
//! replaces the loop with an explicit [`LirOp::Latch`] whose inputs are the
//! enable (the condition under which an arm other than the self-reference
//! is selected) and the data (the value of the selected arm).
//!
//! Only the self-reference of a whole signal through `Mux2` trees (and the
//! buffers between them) is a latch. Any other combinational loop is left in
//! place and reported by [`combinational_loops`].

use crate::lir::{Lir, LirNodeId, LirOp, LirSignalId};
use std::collections::{HashMap, HashSet};

/// Whether the self-reference is selected, as a function of the mux tree
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Hold {
    /// Never (a subtree of data only)
    Never,
    /// Always (a subtree of self-references only)
    Always,
    /// When this 1-bit signal is 1
    When(LirSignalId),
}

/// A subtree's contribution: when it selects the self-reference, and the
/// data it selects otherwise (`None` when it only ever selects the
/// self-reference)
#[derive(Debug, Clone, Copy)]
struct Arm {
    hold: Hold,
    data: Option<LirSignalId>,
}

/// One latch found by [`infer_latches`]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferredLatch {
    /// The latch's output signal, by the source's name for it rather than a
    /// lowering temporary's
    pub signal: String,
    /// Its width in bits
    pub width: u32,
    /// The power-up value the source gives it, if any
    pub initial: Option<u64>,
}

/// Replace every `Mux2` tree that selects its own output (through buffers)
/// on some path with a [`LirOp::Latch`]. Returns the latches made.
///
/// In a loop through several muxes (`if a { x } else if b { y } else { l }`)
/// any of them could hold the state; the latch goes on the one whose value is
/// assigned to a signal (its output buffered into another name, as lowering
/// does for an assignment), so it holds the signal the source names. Loops
/// with no such mux get theirs in a second pass.
pub fn infer_latches(lir: &mut Lir) -> Vec<InferredLatch> {
    let mut made = Vec::new();
    let muxes: Vec<usize> = (0..lir.nodes.len())
        .filter(|&i| matches!(lir.nodes[i].op, LirOp::Mux2 { .. }))
        .collect();
    let assigned: Vec<bool> = muxes
        .iter()
        .map(|&i| buffer_aliases(lir, lir.nodes[i].output).len() > 1)
        .collect();
    let first = muxes.iter().zip(&assigned).filter(|(_, &a)| a);
    let second = muxes.iter().zip(&assigned).filter(|(_, &a)| !a);
    let roots: Vec<usize> = first.chain(second).map(|(&i, _)| i).collect();
    for root in roots {
        // An earlier rewrite may have turned an inner mux of this tree into
        // a latch already; only a mux is a candidate
        let LirOp::Mux2 { width } = lir.nodes[root].op else {
            continue;
        };
        let out = lir.nodes[root].output;
        let aliases = buffer_aliases(lir, out);
        let mut visiting = HashSet::new();
        if !reaches_alias(lir, root, &aliases, &mut visiting) {
            continue;
        }
        let path = lir.nodes[root].path.clone();
        let mut fresh = 0usize;
        let arm = build_arm(lir, root, &aliases, width, &path, &mut fresh);
        let Some(data) = arm.data else {
            // Only ever holds: a signal that never changes, which is not a
            // latch (and has no value). Leave it to the loop report.
            continue;
        };
        let enable = match arm.hold {
            // Never holds: the self-reference is unreachable; a plain mux
            // of the data arms
            Hold::Never => {
                lir.nodes[root].op = LirOp::Buffer { width };
                lir.nodes[root].inputs = vec![data];
                continue;
            }
            Hold::Always => continue,
            Hold::When(hold) => invert(lir, hold, &path, &mut fresh),
        };
        let mut names: Vec<&LirSignalId> = aliases.iter().collect();
        // The source's own name: not a lowering temporary (`_t4`), then
        // the shortest
        names.sort_by_key(|a| {
            let name = &lir.signals[a.0 as usize].name;
            (name.starts_with('_'), name.len(), a.0)
        });
        // Named after the signal it holds, so its cells are too
        let held = &lir.signals[names[0].0 as usize].name;
        let latch_path = match path.rsplit_once('.') {
            Some((scope, _)) => format!("{scope}.{held}"),
            None => held.clone(),
        };
        lir.nodes[root].op = LirOp::Latch { width };
        lir.nodes[root].inputs = vec![enable, data];
        lir.nodes[root].path = latch_path;
        made.push(InferredLatch {
            signal: lir.signals[names[0].0 as usize].name.clone(),
            width,
            initial: names.iter().find_map(|a| lir.signals[a.0 as usize].initial),
        });
    }
    made
}

/// `out` and every signal that is `out` through a chain of buffers of the
/// same width
fn buffer_aliases(lir: &Lir, out: LirSignalId) -> HashSet<LirSignalId> {
    let width = lir.signals[out.0 as usize].width;
    let mut readers: HashMap<LirSignalId, Vec<LirSignalId>> = HashMap::new();
    for node in &lir.nodes {
        if let LirOp::Buffer { width: w } = node.op {
            if w == width && node.inputs.len() == 1 {
                readers.entry(node.inputs[0]).or_default().push(node.output);
            }
        }
    }
    let mut aliases = HashSet::from([out]);
    let mut todo = vec![out];
    while let Some(s) = todo.pop() {
        for &r in readers.get(&s).into_iter().flatten() {
            if aliases.insert(r) {
                todo.push(r);
            }
        }
    }
    aliases
}

/// The node through which a data arm's value comes: past buffers of the
/// same width, unless the signal is a self-reference
fn arm_driver(lir: &Lir, mut sig: LirSignalId, aliases: &HashSet<LirSignalId>) -> Option<usize> {
    for _ in 0..lir.nodes.len() {
        if aliases.contains(&sig) {
            return None;
        }
        let node = lir.signals[sig.0 as usize].driver?.0 as usize;
        match lir.nodes[node].op {
            LirOp::Buffer { width }
                if lir.nodes[node].inputs.len() == 1
                    && lir.signals[lir.nodes[node].inputs[0].0 as usize].width == width =>
            {
                sig = lir.nodes[node].inputs[0];
            }
            _ => return Some(node),
        }
    }
    None
}

/// Whether the data arms of the mux tree at `node` reach a self-reference
fn reaches_alias(
    lir: &Lir,
    node: usize,
    aliases: &HashSet<LirSignalId>,
    visiting: &mut HashSet<usize>,
) -> bool {
    if !visiting.insert(node) {
        return false;
    }
    let n = &lir.nodes[node];
    if !matches!(n.op, LirOp::Mux2 { .. }) || n.inputs.len() != 3 {
        return false;
    }
    n.inputs[1..].iter().any(|&leg| {
        aliases.contains(&leg)
            || arm_driver(lir, leg, aliases)
                .is_some_and(|d| reaches_alias(lir, d, aliases, visiting))
    })
}

/// The hold condition and data of the mux tree at `node`, adding the nodes
/// that compute them. A subtree without self-references is data as it is.
fn build_arm(
    lir: &mut Lir,
    node: usize,
    aliases: &HashSet<LirSignalId>,
    width: u32,
    path: &str,
    fresh: &mut usize,
) -> Arm {
    let inputs = lir.nodes[node].inputs.clone();
    let (sel, legs) = (inputs[0], [inputs[1], inputs[2]]);
    let arms = legs.map(|leg| {
        if aliases.contains(&leg) {
            return Arm {
                hold: Hold::Always,
                data: None,
            };
        }
        match arm_driver(lir, leg, aliases) {
            Some(d) if reaches_alias(lir, d, aliases, &mut HashSet::new()) => {
                build_arm(lir, d, aliases, width, path, fresh)
            }
            _ => Arm {
                hold: Hold::Never,
                data: Some(leg),
            },
        }
    });
    let [low, high] = arms;
    let hold = match (low.hold, high.hold) {
        (a, b) if a == b => a,
        // sel ? high : low, over hold conditions
        (Hold::Never, Hold::Always) => Hold::When(sel),
        (Hold::Always, Hold::Never) => Hold::When(invert(lir, sel, path, fresh)),
        (a, b) => {
            let a = hold_signal(lir, a, path, fresh);
            let b = hold_signal(lir, b, path, fresh);
            Hold::When(add(
                lir,
                LirOp::Mux2 { width: 1 },
                vec![sel, a, b],
                1,
                path,
                fresh,
            ))
        }
    };
    let data = match (low.data, high.data) {
        (Some(a), Some(b)) => Some(add(
            lir,
            LirOp::Mux2 { width },
            vec![sel, a, b],
            width,
            path,
            fresh,
        )),
        (a, b) => a.or(b),
    };
    Arm { hold, data }
}

/// A 1-bit signal of a hold condition
fn hold_signal(lir: &mut Lir, hold: Hold, path: &str, fresh: &mut usize) -> LirSignalId {
    match hold {
        Hold::When(s) => s,
        Hold::Never | Hold::Always => add(
            lir,
            LirOp::Constant {
                width: 1,
                value: u64::from(hold == Hold::Always),
            },
            vec![],
            1,
            path,
            fresh,
        ),
    }
}

/// `!s`, reusing the input of an inverter rather than inverting twice
fn invert(lir: &mut Lir, s: LirSignalId, path: &str, fresh: &mut usize) -> LirSignalId {
    if let Some(LirNodeId(d)) = lir.signals[s.0 as usize].driver {
        let node = &lir.nodes[d as usize];
        if matches!(node.op, LirOp::Not { width: 1 }) && node.inputs.len() == 1 {
            return node.inputs[0];
        }
    }
    add(lir, LirOp::Not { width: 1 }, vec![s], 1, path, fresh)
}

/// Add a node computing `op` over `inputs` into a new signal
fn add(
    lir: &mut Lir,
    op: LirOp,
    inputs: Vec<LirSignalId>,
    width: u32,
    path: &str,
    fresh: &mut usize,
) -> LirSignalId {
    let name = format!("{path}.latch{}", *fresh);
    *fresh += 1;
    let out = lir.add_signal(name.clone(), width);
    lir.add_node(op, inputs, out, name);
    out
}

/// Names of signals on combinational loops: cycles that pass through no
/// register, latch or other element that holds state or is mapped as is
/// (memories, self-timed control). Such a loop has no defined order, so
/// logic synthesis cannot implement it as written.
pub fn combinational_loops(lir: &Lir) -> Vec<String> {
    // Threshold gates hold their output too (hysteresis)
    let breaks =
        |op: &LirOp| op.is_sequential() || matches!(op, LirOp::Th12 { .. } | LirOp::Th22 { .. });
    // Tarjan's strongly connected components over nodes, edges from a
    // node to the nodes reading its output
    let n = lir.nodes.len();
    let mut readers: Vec<Vec<usize>> = vec![Vec::new(); n];
    for (i, node) in lir.nodes.iter().enumerate() {
        if breaks(&node.op) {
            continue;
        }
        for &input in &node.inputs {
            if let Some(LirNodeId(d)) = lir.signals[input.0 as usize].driver {
                let d = d as usize;
                if d < n && !breaks(&lir.nodes[d].op) {
                    readers[d].push(i);
                }
            }
        }
    }
    let mut index = vec![usize::MAX; n];
    let mut low = vec![0usize; n];
    let mut on_stack = vec![false; n];
    let mut stack = Vec::new();
    let mut next = 0usize;
    let mut looped = Vec::new();
    for start in 0..n {
        if index[start] != usize::MAX {
            continue;
        }
        // Iterative DFS: (node, next reader to visit)
        let mut work = vec![(start, 0usize)];
        index[start] = next;
        low[start] = next;
        next += 1;
        stack.push(start);
        on_stack[start] = true;
        while let Some(&mut (v, ref mut k)) = work.last_mut() {
            if *k < readers[v].len() {
                let w = readers[v][*k];
                *k += 1;
                if index[w] == usize::MAX {
                    index[w] = next;
                    low[w] = next;
                    next += 1;
                    stack.push(w);
                    on_stack[w] = true;
                    work.push((w, 0));
                } else if on_stack[w] {
                    low[v] = low[v].min(index[w]);
                }
                continue;
            }
            work.pop();
            if let Some(&(u, _)) = work.last() {
                low[u] = low[u].min(low[v]);
            }
            if low[v] == index[v] {
                let mut scc = Vec::new();
                while let Some(w) = stack.pop() {
                    on_stack[w] = false;
                    scc.push(w);
                    if w == v {
                        break;
                    }
                }
                let self_loop = scc.len() == 1 && readers[v].contains(&v);
                if scc.len() > 1 || self_loop {
                    let mut names: Vec<String> = scc
                        .iter()
                        .map(|&i| lir.signals[lir.nodes[i].output.0 as usize].name.clone())
                        .collect();
                    names.sort();
                    looped.push(names.join(", "));
                }
            }
        }
    }
    looped
}

#[cfg(test)]
mod tests {
    use super::*;

    fn latch_of(lir: &Lir, name: &str) -> (Vec<LirSignalId>, u32) {
        let sig = lir.get_signal(name).unwrap();
        let node = &lir.nodes[sig.driver.unwrap().0 as usize];
        match node.op {
            LirOp::Latch { width } => (node.inputs.clone(), width),
            ref op => panic!("{name} is driven by {op:?}, not a latch"),
        }
    }

    /// `l = if g { d } else { l }` through a buffer, as lowering makes it
    #[test]
    fn simple_latch() {
        let mut lir = Lir::new("t".into());
        let g = lir.add_input("g".into(), 1);
        let d = lir.add_input("d".into(), 8);
        let m = lir.add_signal("m".into(), 8);
        let l = lir.add_signal("l".into(), 8);
        lir.add_node(LirOp::Mux2 { width: 8 }, vec![g, l, d], m, "t.mux".into());
        lir.add_node(LirOp::Buffer { width: 8 }, vec![m], l, "t.buf".into());
        assert_eq!(combinational_loops(&lir).len(), 1);
        let made = infer_latches(&mut lir);
        assert_eq!(made.len(), 1);
        let (inputs, width) = latch_of(&lir, "m");
        assert_eq!(width, 8);
        // enable = !(hold) = !(!g) = g
        assert_eq!(inputs, vec![g, d]);
        assert!(combinational_loops(&lir).is_empty());
    }

    /// The self-reference on the true leg: `l = if h { l } else { d }`
    #[test]
    fn hold_on_true_leg() {
        let mut lir = Lir::new("t".into());
        let h = lir.add_input("h".into(), 1);
        let d = lir.add_input("d".into(), 1);
        let l = lir.add_signal("l".into(), 1);
        lir.add_node(LirOp::Mux2 { width: 1 }, vec![h, d, l], l, "t.mux".into());
        infer_latches(&mut lir);
        let (inputs, _) = latch_of(&lir, "l");
        assert_eq!(inputs[1], d);
        // enable = !h
        let en = &lir.nodes[lir.signals[inputs[0].0 as usize].driver.unwrap().0 as usize];
        assert!(matches!(en.op, LirOp::Not { .. }));
        assert_eq!(en.inputs, vec![h]);
    }

    /// `l = if a { x } else if b { y } else { l }`: enable a | b, data the
    /// selected arm
    #[test]
    fn nested_arms() {
        let mut lir = Lir::new("t".into());
        let a = lir.add_input("a".into(), 1);
        let b = lir.add_input("b".into(), 1);
        let x = lir.add_input("x".into(), 4);
        let y = lir.add_input("y".into(), 4);
        let inner = lir.add_signal("inner".into(), 4);
        let m = lir.add_signal("t.mux_out".into(), 4);
        let l = lir.add_signal("l".into(), 4);
        lir.add_node(
            LirOp::Mux2 { width: 4 },
            vec![b, l, y],
            inner,
            "t.in".into(),
        );
        lir.add_node(
            LirOp::Mux2 { width: 4 },
            vec![a, inner, x],
            m,
            "t.out".into(),
        );
        lir.add_node(LirOp::Buffer { width: 4 }, vec![m], l, "t.assign".into());
        let made = infer_latches(&mut lir);
        assert_eq!(made.len(), 1);
        assert_eq!(made[0].signal, "l");
        let (inputs, _) = latch_of(&lir, "t.mux_out");
        // enable = a | b (as !(!a & !b) in mux form), data = a ? x : y
        let data = &lir.nodes[lir.signals[inputs[1].0 as usize].driver.unwrap().0 as usize];
        assert!(matches!(data.op, LirOp::Mux2 { width: 4 }));
        assert_eq!(data.inputs, vec![a, y, x]);
        assert_eq!(made[0].initial, None);
        assert!(combinational_loops(&lir).is_empty());
    }

    /// A loop through other logic is not a latch and is reported
    #[test]
    fn other_loops_reported() {
        let mut lir = Lir::new("t".into());
        let a = lir.add_input("a".into(), 1);
        let x = lir.add_signal("x".into(), 1);
        let y = lir.add_signal("y".into(), 1);
        lir.add_node(LirOp::And { width: 1 }, vec![a, y], x, "t.and".into());
        lir.add_node(LirOp::Not { width: 1 }, vec![x], y, "t.not".into());
        assert!(infer_latches(&mut lir).is_empty());
        assert_eq!(combinational_loops(&lir), vec!["x, y".to_string()]);
    }
}
