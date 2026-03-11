//! DFF Functional Decomposition via AIG Cofactoring
//!
//! For each latch in the AIG, decomposes its data function `F(inputs, Q)` into
//! enable, sync-reset, and sync-set signals using Boolean cofactors:
//!
//! ```text
//! E  = ~(F|Q=0 XOR F|Q=1)     -- enable: where F depends on Q
//! F' = F|R=0                   -- function with reset deasserted
//! ```
//!
//! This is purely functional — it works regardless of how deeply nested the
//! MUX structure is, unlike structural pattern matching.
//!
//! Target decomposition for iCE40 SB_DFFESR:
//!   `F = S ? 1 : (R ? 0 : (E ? D : Q))`

use super::aig::{Aig, AigLit, AigNode, AigNodeId};
use std::collections::HashMap;

/// Result of decomposing a latch's data function.
#[derive(Debug, Clone)]
pub struct LatchDecomp {
    /// Enable signal (None = always enabled)
    pub enable: Option<AigLit>,
    /// New data input (with enable factored out)
    pub data: AigLit,
    /// Synchronous reset signal (None = no sync reset)
    pub sync_reset: Option<AigLit>,
    /// Reset value (true=set-to-1, false=reset-to-0)
    pub reset_value: bool,
}

/// Decompose all latches in an AIG.
///
/// Analyzes each latch's data cone to extract enable and sync-reset signals
/// using Boolean cofactoring. Does NOT modify the AIG — only builds new nodes
/// (via structural hashing) for the decomposed sub-functions.
///
/// Returns a map from latch AigNodeId to its decomposition. Latches that
/// can't be decomposed (no enable, no reset) are omitted.
pub fn decompose_latches(aig: &mut Aig) -> HashMap<AigNodeId, LatchDecomp> {
    let mut results = HashMap::new();

    // Collect latch info to avoid borrow issues
    let latches: Vec<(AigNodeId, AigLit, Option<AigNodeId>)> = aig
        .iter_latches()
        .map(|(id, node)| {
            if let AigNode::Latch { data, reset, .. } = node {
                (id, data.clone(), *reset)
            } else {
                unreachable!()
            }
        })
        .collect();

    for (latch_id, data_lit, reset_node) in &latches {
        let q_lit = AigLit::new(*latch_id);

        // Check if Q is in the transitive fanin of D
        if !is_in_fanin(aig, data_lit.node, *latch_id) {
            continue; // Q doesn't feed back — no enable possible
        }

        // Step 1: Try sync reset detection
        // Look for the latch's reset signal where F|reset=1 = constant
        let mut sync_reset: Option<(AigLit, bool)> = None;
        let mut data_after_reset = *data_lit;

        if let Some(reset_id) = *reset_node {
            let reset_lit = AigLit::new(reset_id);
            let cofactor_r1 = cofactor(aig, *data_lit, reset_lit, true);
            if cofactor_r1.const_value() == Some(false) {
                // Active-high sync reset to 0
                let cofactor_r0 = cofactor(aig, *data_lit, reset_lit, false);
                sync_reset = Some((reset_lit, false));
                data_after_reset = cofactor_r0;
            } else if cofactor_r1.const_value() == Some(true) {
                // Active-high sync set to 1
                let cofactor_r0 = cofactor(aig, *data_lit, reset_lit, false);
                sync_reset = Some((reset_lit, true));
                data_after_reset = cofactor_r0;
            }
        }

        // Step 2: Enable detection via Boolean difference
        // dF/dQ = F|Q=1 XOR F|Q=0;  E = ~(dF/dQ)
        let f_q0 = cofactor(aig, data_after_reset, q_lit, false);
        let f_q1 = cofactor(aig, data_after_reset, q_lit, true);
        let bool_diff = build_xor(aig, f_q0, f_q1);

        let enable = if bool_diff.const_value() == Some(false) {
            // dF/dQ = 0: F doesn't depend on Q (after reset peeling).
            // Q dropped out — no enable pattern.
            None
        } else if bool_diff.const_value() == Some(true) {
            // dF/dQ = 1: F always depends on Q. This means F = Q or F = ~Q,
            // not a MUX enable pattern.
            None
        } else {
            // Non-trivial enable: E = ~bool_diff
            let e_lit = bool_diff.invert();
            Some(e_lit)
        };

        if enable.is_none() && sync_reset.is_none() {
            continue; // Nothing to decompose
        }

        // Compute D_new: the data function with enable and Q factored out.
        // F = E*D_new + ~E*Q, so F|Q=0 = E*D_new.
        // D_new = F|Q=0 with E forced to 1 (cofactor with E=1).
        let new_data = if let Some(e_lit) = enable {
            cofactor(aig, f_q0, e_lit, true)
        } else {
            data_after_reset
        };

        results.insert(
            *latch_id,
            LatchDecomp {
                enable,
                data: new_data,
                sync_reset: sync_reset.map(|(s, _)| s),
                reset_value: sync_reset.map(|(_, v)| v).unwrap_or(false),
            },
        );
    }

    results
}

/// Check if `target` is in the transitive fanin of `node`.
fn is_in_fanin(aig: &Aig, node: AigNodeId, target: AigNodeId) -> bool {
    let mut visited = HashMap::new();
    is_in_fanin_rec(aig, node, target, &mut visited)
}

fn is_in_fanin_rec(
    aig: &Aig,
    node: AigNodeId,
    target: AigNodeId,
    visited: &mut HashMap<AigNodeId, bool>,
) -> bool {
    if node == target {
        return true;
    }
    if node == AigNodeId::FALSE {
        return false;
    }
    if let Some(&result) = visited.get(&node) {
        return result;
    }
    let result = match aig.get_node(node) {
        Some(AigNode::And { left, right }) => {
            is_in_fanin_rec(aig, left.node, target, visited)
                || is_in_fanin_rec(aig, right.node, target, visited)
        }
        _ => false,
    };
    visited.insert(node, result);
    result
}

/// Cofactor an AIG literal with respect to a variable.
///
/// Returns a new AIG literal representing `F|var=value`.
/// Builds new AND nodes as needed (with structural hashing via add_and).
fn cofactor(aig: &mut Aig, lit: AigLit, var: AigLit, value: bool) -> AigLit {
    let mut cache: HashMap<AigNodeId, AigLit> = HashMap::new();
    cofactor_rec(aig, lit, var, value, &mut cache)
}

fn cofactor_rec(
    aig: &mut Aig,
    lit: AigLit,
    var: AigLit,
    value: bool,
    cache: &mut HashMap<AigNodeId, AigLit>,
) -> AigLit {
    // Base: constant
    if lit.node == AigNodeId::FALSE {
        return lit;
    }

    // Check if this literal IS the variable we're cofactoring
    if lit.node == var.node {
        let effective_inv = lit.inverted ^ var.inverted;
        let const_val = value ^ effective_inv;
        return if const_val {
            AigLit::true_lit()
        } else {
            AigLit::false_lit()
        };
    }

    // Check cache (keyed by positive node, handle inversion outside)
    if let Some(&cached) = cache.get(&lit.node) {
        return if lit.inverted {
            cached.invert()
        } else {
            cached
        };
    }

    // Only recurse into AND nodes
    let result = match aig.get_node(lit.node) {
        Some(AigNode::And { left, right }) => {
            let left = *left;
            let right = *right;
            let new_left = cofactor_rec(aig, left, var, value, cache);
            let new_right = cofactor_rec(aig, right, var, value, cache);
            aig.add_and(new_left, new_right)
        }
        _ => {
            // Input, Latch, Barrier — not dependent on var
            AigLit::new(lit.node)
        }
    };

    cache.insert(lit.node, result);

    if lit.inverted {
        result.invert()
    } else {
        result
    }
}

/// Build XOR: a XOR b = ~(~(a & ~b) & ~(~a & b))
fn build_xor(aig: &mut Aig, a: AigLit, b: AigLit) -> AigLit {
    let a_and_nb = aig.add_and(a, b.invert());
    let na_and_b = aig.add_and(a.invert(), b);
    let nor = aig.add_and(a_and_nb.invert(), na_and_b.invert());
    nor.invert()
}

/// Build MUX: sel ? d1 : d0 = (sel & d1) | (~sel & d0)
pub fn build_mux(aig: &mut Aig, sel: AigLit, d1: AigLit, d0: AigLit) -> AigLit {
    let sel_and_d1 = aig.add_and(sel, d1);
    let nsel_and_d0 = aig.add_and(sel.invert(), d0);
    let nor = aig.add_and(sel_and_d1.invert(), nsel_and_d0.invert());
    nor.invert()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Build a simple AIG with a latch whose data input is MUX(E, D, Q)
    fn make_mux_enable_aig() -> Aig {
        let mut aig = Aig::new("test".to_string());

        let e_node = aig.add_input("e".to_string(), None);
        let d_node = aig.add_input("d".to_string(), None);
        let q_node = aig.add_latch(AigLit::false_lit(), None, None, None);

        let e_lit = AigLit::new(e_node);
        let d_lit = AigLit::new(d_node);
        let q_lit = AigLit::new(q_node);

        // MUX(E, D, Q) = (E & D) | (~E & Q)
        let mux = build_mux(&mut aig, e_lit, d_lit, q_lit);
        aig.update_latch_data(q_node, mux);

        aig.add_output("q_out".to_string(), q_lit);
        aig
    }

    #[test]
    fn test_enable_detection() {
        let mut aig = make_mux_enable_aig();
        let decomps = decompose_latches(&mut aig);

        assert_eq!(decomps.len(), 1, "Should detect one enable pattern");
        let decomp = decomps.values().next().unwrap();
        assert!(decomp.enable.is_some(), "Should have enable signal");
        assert!(decomp.sync_reset.is_none(), "Should have no sync reset");
    }

    #[test]
    fn test_deeply_nested_enable() {
        // Build: F = MUX(c1, MUX(c2, D, Q), Q)
        // Equivalent to: MUX(c1 & c2, D, Q)
        let mut aig = Aig::new("test".to_string());

        let c1_node = aig.add_input("c1".to_string(), None);
        let c2_node = aig.add_input("c2".to_string(), None);
        let d_node = aig.add_input("d".to_string(), None);
        let q_node = aig.add_latch(AigLit::false_lit(), None, None, None);

        let c1 = AigLit::new(c1_node);
        let c2 = AigLit::new(c2_node);
        let d = AigLit::new(d_node);
        let q = AigLit::new(q_node);

        let inner = build_mux(&mut aig, c2, d, q);
        let outer = build_mux(&mut aig, c1, inner, q);
        aig.update_latch_data(q_node, outer);
        aig.add_output("q_out".to_string(), q);

        let decomps = decompose_latches(&mut aig);

        assert_eq!(decomps.len(), 1);
        let decomp = decomps.values().next().unwrap();
        assert!(decomp.enable.is_some(), "Should detect enable from nested MUX");
    }

    #[test]
    fn test_no_enable_simple_dff() {
        let mut aig = Aig::new("test".to_string());
        let d_node = aig.add_input("d".to_string(), None);
        let q_node = aig.add_latch(AigLit::new(d_node), None, None, None);
        aig.add_output("q_out".to_string(), AigLit::new(q_node));

        let decomps = decompose_latches(&mut aig);
        assert!(decomps.is_empty(), "No decomposition for simple DFF");
    }

    #[test]
    fn test_three_level_nested_enable() {
        // F = MUX(c1, MUX(c2, MUX(c3, D, Q), Q), Q)
        // Enable = c1 & c2 & c3
        let mut aig = Aig::new("test".to_string());

        let c1_node = aig.add_input("c1".to_string(), None);
        let c2_node = aig.add_input("c2".to_string(), None);
        let c3_node = aig.add_input("c3".to_string(), None);
        let d_node = aig.add_input("d".to_string(), None);
        let q_node = aig.add_latch(AigLit::false_lit(), None, None, None);

        let c1 = AigLit::new(c1_node);
        let c2 = AigLit::new(c2_node);
        let c3 = AigLit::new(c3_node);
        let d = AigLit::new(d_node);
        let q = AigLit::new(q_node);

        let inner = build_mux(&mut aig, c3, d, q);
        let mid = build_mux(&mut aig, c2, inner, q);
        let outer = build_mux(&mut aig, c1, mid, q);
        aig.update_latch_data(q_node, outer);
        aig.add_output("q_out".to_string(), q);

        let decomps = decompose_latches(&mut aig);
        assert_eq!(decomps.len(), 1);
        assert!(decomps.values().next().unwrap().enable.is_some());
    }

    #[test]
    fn test_enable_with_multiple_data_paths() {
        // F = MUX(c1, D1, MUX(c2, D2, Q))
        // Enable = c1 | c2, Data = MUX(c1, D1, D2) when enabled
        let mut aig = Aig::new("test".to_string());

        let c1_node = aig.add_input("c1".to_string(), None);
        let c2_node = aig.add_input("c2".to_string(), None);
        let d1_node = aig.add_input("d1".to_string(), None);
        let d2_node = aig.add_input("d2".to_string(), None);
        let q_node = aig.add_latch(AigLit::false_lit(), None, None, None);

        let c1 = AigLit::new(c1_node);
        let c2 = AigLit::new(c2_node);
        let d1 = AigLit::new(d1_node);
        let d2 = AigLit::new(d2_node);
        let q = AigLit::new(q_node);

        let inner = build_mux(&mut aig, c2, d2, q);
        let outer = build_mux(&mut aig, c1, d1, inner);
        aig.update_latch_data(q_node, outer);
        aig.add_output("q_out".to_string(), q);

        let decomps = decompose_latches(&mut aig);
        assert_eq!(decomps.len(), 1);
        let decomp = decomps.values().next().unwrap();
        assert!(decomp.enable.is_some(), "Should detect enable with multiple data paths");
    }
}
