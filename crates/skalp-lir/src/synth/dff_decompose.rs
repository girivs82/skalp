//! Flip-flop enable extraction.
//!
//! A register that keeps its value under some condition reads its own output
//! back through its data logic: `F(x, Q) = E(x) ? D(x) : Q`. Built that way,
//! the hold path costs LUT inputs (`Q` and the conditions) and often a LUT
//! level in front of the flip-flop. A flip-flop with a clock enable holds by
//! itself: this pass finds `E` and `D` so the writer can emit one (`DffE`),
//! leaving `D` free of `Q` and of the hold conditions.
//!
//! The decomposition is exact and structural. Every node of the latch's data
//! cone that depends on `Q` is written as `E ? D : Q ^ p`: a node that does
//! not depend on `Q` loads itself (`E = 1`); `Q` holds (`E = 0`); a
//! multiplexer takes `E` and `D` from its branches through its select; an
//! AND (with inverted inputs, any AND/OR) of such forms is again of that
//! form, because each operand is either a constant or `Q ^ p` for a given
//! input. A cone that mixes `Q` and `!Q` (a toggle, an XOR) has no such form
//! and keeps its data logic.

use super::aig::{Aig, AigLit, AigNode, AigNodeId};
use std::collections::{HashMap, HashSet};

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

/// Most nodes depending on `Q` that one latch's decomposition walks; a
/// larger cone (wide feedback arithmetic, say) keeps its data logic.
const MAX_FEEDBACK_CONE: usize = 512;

/// Extract a clock enable for every latch whose data function holds its
/// value under some condition (see the module documentation).
///
/// The returned enable and data are those of the cell the writer builds:
///
/// - Without a reset, `F = E ? D : Q`.
/// - With an asynchronous reset, the reset pin stays and overrides the
///   enable (as a `DffRE` does), so `E` and `D` only have to match `F` while
///   the reset is released: the reset reads as 0 in them.
/// - With a synchronous reset, the cell has no reset pin: the reset loads
///   the reset value through the enable and data,
///   `E' = rst | E`, `D' = rst ? init : D`.
///
/// Only builds new nodes (with structural hashing); the latches keep their
/// data. Returns, per decomposed latch, its enable and its new data.
pub fn decompose_latches(aig: &mut Aig) -> HashMap<AigNodeId, LatchDecomp> {
    let latches: Vec<(AigNodeId, AigLit, Option<AigLit>, bool, bool)> = aig
        .iter_latches()
        .filter_map(|(id, node)| match node {
            AigNode::Latch {
                data,
                init,
                reset,
                sync_reset,
                ..
            } => Some((id, *data, *reset, *sync_reset, *init == Some(true))),
            _ => None,
        })
        .collect();

    let mut results = HashMap::new();
    for (latch, data, reset, sync_reset, init) in latches {
        let mut peeler = Peeler::new(latch, reset);
        if reset.is_some_and(|r| peeler.depends_on_q(aig, r.node)) {
            continue;
        }
        let Some(form) = peeler.peel_lit(aig, data) else {
            continue;
        };
        // F = E ? D : Q, not !Q; E neither always (no hold) nor never
        if form.polarity != Some(false) || form.enable.is_const() {
            continue;
        }
        let q = AigLit::new(latch);
        let (enable, new_data, reference, released) = match reset {
            Some(rst) if sync_reset => {
                let value = if init {
                    AigLit::true_lit()
                } else {
                    AigLit::false_lit()
                };
                let enable = aig.add_or(rst, form.enable);
                let new_data = aig.add_mux(rst, value, form.data);
                // The writer loads the reset value over F
                let next = aig.add_mux(rst, value, data);
                (enable, new_data, next, None)
            }
            _ => (form.enable, form.data, data, reset),
        };
        let rebuilt = build_mux(aig, enable, new_data, q);
        if !verify_equivalence(aig, reference, rebuilt, latch, released) {
            // The algebra is exact; a mismatch is a bug, never a reason to
            // build a wrong register
            tracing::warn!(
                "[DFF_ENABLE] latch {:?}: enable decomposition failed simulation, kept as is",
                latch
            );
            continue;
        }
        results.insert(
            latch,
            LatchDecomp {
                enable: Some(enable),
                data: new_data,
                sync_reset: None,
                reset_value: false,
            },
        );
    }
    results
}

/// A function written as `enable ? data : Q ^ polarity`, where `polarity`
/// is `None` for a function that never passes `Q` through (`enable` is 1).
#[derive(Debug, Clone, Copy)]
struct HoldForm {
    enable: AigLit,
    data: AigLit,
    polarity: Option<bool>,
}

impl HoldForm {
    fn invert(self) -> Self {
        HoldForm {
            enable: self.enable,
            data: self.data.invert(),
            polarity: self.polarity.map(|p| !p),
        }
    }
}

/// Both polarities of `Q` agree (or one side has none).
fn merge_polarity(a: Option<bool>, b: Option<bool>) -> Option<Option<bool>> {
    match (a, b) {
        (None, p) | (p, None) => Some(p),
        (Some(a), Some(b)) if a == b => Some(Some(a)),
        _ => None,
    }
}

struct Peeler {
    latch: AigNodeId,
    /// An asynchronous reset literal, read as released (0)
    released: Option<AigLit>,
    /// Whether a node's cone contains the latch output
    depends: HashMap<AigNodeId, bool>,
    /// Hold form of each node depending on `Q` (`None`: it has none)
    forms: HashMap<AigNodeId, Option<HoldForm>>,
}

impl Peeler {
    fn new(latch: AigNodeId, released: Option<AigLit>) -> Self {
        Self {
            latch,
            released,
            depends: HashMap::new(),
            forms: HashMap::new(),
        }
    }

    fn depends_on_q(&mut self, aig: &Aig, node: AigNodeId) -> bool {
        if node == self.latch {
            return true;
        }
        if let Some(&d) = self.depends.get(&node) {
            return d;
        }
        // Iterative post-order: data cones can be deep
        let mut stack = vec![(node, false)];
        while let Some((n, expanded)) = stack.pop() {
            if n == self.latch || self.depends.contains_key(&n) {
                continue;
            }
            match aig.get_node(n) {
                Some(AigNode::And { left, right }) => {
                    let (l, r) = (left.node, right.node);
                    if expanded {
                        let dep = |x: AigNodeId, m: &HashMap<AigNodeId, bool>| {
                            x == self.latch || m.get(&x).copied().unwrap_or(false)
                        };
                        let d = dep(l, &self.depends) || dep(r, &self.depends);
                        self.depends.insert(n, d);
                    } else {
                        stack.push((n, true));
                        stack.push((l, false));
                        stack.push((r, false));
                    }
                }
                // Inputs, other latches, barriers, constants
                _ => {
                    self.depends.insert(n, false);
                }
            }
        }
        self.depends[&node]
    }

    /// The value of `lit` while the reset is released, when it is the reset.
    fn released_value(&self, lit: AigLit) -> Option<bool> {
        // The reset literal reads 0 when released
        self.released
            .filter(|r| r.node == lit.node)
            .map(|r| r.inverted != lit.inverted)
    }

    fn peel_lit(&mut self, aig: &mut Aig, lit: AigLit) -> Option<HoldForm> {
        if let Some(value) = self.released_value(lit) {
            return Some(HoldForm {
                enable: AigLit::true_lit(),
                data: if value {
                    AigLit::true_lit()
                } else {
                    AigLit::false_lit()
                },
                polarity: None,
            });
        }
        if !self.depends_on_q(aig, lit.node) {
            return Some(HoldForm {
                enable: AigLit::true_lit(),
                data: lit,
                polarity: None,
            });
        }
        let form = if lit.node == self.latch {
            HoldForm {
                enable: AigLit::false_lit(),
                data: AigLit::false_lit(),
                polarity: Some(false),
            }
        } else {
            self.peel_node(aig, lit.node)?
        };
        Some(if lit.inverted { form.invert() } else { form })
    }

    /// Hold form of an AND node whose cone contains `Q`.
    fn peel_node(&mut self, aig: &mut Aig, node: AigNodeId) -> Option<HoldForm> {
        if let Some(&form) = self.forms.get(&node) {
            return form;
        }
        if self.forms.len() >= MAX_FEEDBACK_CONE {
            return None;
        }
        // Visited, not yet known: a revisit means a cycle, which an AIG has not
        self.forms.insert(node, None);
        let form = self.peel_and(aig, node);
        self.forms.insert(node, form);
        form
    }

    fn peel_and(&mut self, aig: &mut Aig, node: AigNodeId) -> Option<HoldForm> {
        let Some(&AigNode::And { left, right }) = aig.get_node(node) else {
            return None;
        };

        // node = !(s ? a : b), s not depending on Q: select from the
        // branches through s
        if let Some((select, a, b)) = as_inverted_mux(aig, left, right)
            .filter(|&(select, _, _)| !self.depends_on_q(aig, select.node))
        {
            // A released reset selects one branch
            if let Some(s) = self.released_value(select) {
                return Some(self.peel_lit(aig, if s { a } else { b })?.invert());
            }
            let fa = self.peel_lit(aig, a)?;
            let fb = self.peel_lit(aig, b)?;
            let polarity = merge_polarity(fa.polarity, fb.polarity)?;
            let enable = aig.add_mux(select, fa.enable, fb.enable);
            // Where a branch never loads, its data is never read
            let data = if fb.enable.const_value() == Some(false) {
                fa.data
            } else if fa.enable.const_value() == Some(false) {
                fb.data
            } else {
                aig.add_mux(select, fa.data, fb.data)
            };
            let mux = HoldForm {
                enable,
                data,
                polarity,
            };
            return Some(mux.invert());
        }

        let fl = self.peel_lit(aig, left)?;
        let fr = self.peel_lit(aig, right)?;
        let polarity = merge_polarity(fl.polarity, fr.polarity)?;
        // l & r: where one operand is a constant 0 the AND loads 0; where
        // both load, it loads both data ANDed; elsewhere it passes Q ^ p.
        let (enable, data) = match (fl.polarity, fr.polarity) {
            // One side does not see Q: it is its own data
            (None, _) => (
                aig.add_or(fl.data.invert(), fr.enable),
                aig.add_and(fl.data, fr.data),
            ),
            (_, None) => (
                aig.add_or(fr.data.invert(), fl.enable),
                aig.add_and(fr.data, fl.data),
            ),
            _ => {
                let both = aig.add_and(fl.enable, fr.enable);
                let left_zero = aig.add_and(fl.enable, fl.data.invert());
                let right_zero = aig.add_and(fr.enable, fr.data.invert());
                let zero = aig.add_or(left_zero, right_zero);
                (aig.add_or(both, zero), aig.add_and(fl.data, fr.data))
            }
        };
        Some(HoldForm {
            enable,
            data,
            polarity,
        })
    }
}

/// `left & right` as `!(s ? a : b)`: both operands inverted ANDs, one with
/// `s` and one with `!s`.
fn as_inverted_mux(aig: &Aig, left: AigLit, right: AigLit) -> Option<(AigLit, AigLit, AigLit)> {
    if !left.inverted || !right.inverted {
        return None;
    }
    let operands = |lit: AigLit| match aig.get_node(lit.node) {
        Some(&AigNode::And { left, right }) => Some([left, right]),
        _ => None,
    };
    let l = operands(left)?;
    let r = operands(right)?;
    for i in 0..2 {
        for j in 0..2 {
            if l[i] == r[j].invert() {
                // !(s & a) & !(!s & b)
                return Some((l[i], l[1 - i], r[1 - j]));
            }
        }
    }
    None
}

/// Verify that two AIG literals are functionally equivalent using random simulation.
/// Checks 256 random input patterns (4 rounds of 64-bit parallel sim), with
/// `released` (an asynchronous reset literal) held at 0.
fn verify_equivalence(
    aig: &Aig,
    original: AigLit,
    reconstructed: AigLit,
    latch_id: AigNodeId,
    released: Option<AigLit>,
) -> bool {
    // Collect all input nodes (PIs and latches) in the transitive fanin of both functions
    let mut inputs_orig = HashSet::new();
    let mut inputs_recon = HashSet::new();
    collect_inputs(aig, original.node, &mut inputs_orig, &mut HashSet::new());
    collect_inputs(
        aig,
        reconstructed.node,
        &mut inputs_recon,
        &mut HashSet::new(),
    );
    let mut all_inputs = inputs_orig.clone();
    all_inputs.extend(&inputs_recon);
    let inputs: Vec<AigNodeId> = all_inputs.into_iter().collect();

    // Debug: show inputs unique to each side
    let only_orig: Vec<_> = inputs_orig.difference(&inputs_recon).collect();
    let only_recon: Vec<_> = inputs_recon.difference(&inputs_orig).collect();
    if !only_orig.is_empty() || !only_recon.is_empty() {
        tracing::trace!(
            "[DFF_VERIFY] latch {:?}: orig_only_inputs={:?}, recon_only_inputs={:?}",
            latch_id,
            only_orig,
            only_recon
        );
    }

    // Use a simple PRNG (xorshift64) for reproducibility
    let mut rng_state: u64 = 0xDEAD_BEEF_CAFE_BABEu64 ^ (latch_id.0 as u64);

    for round in 0..4 {
        // Assign random 64-bit values to each input (64 patterns in parallel)
        let mut values: HashMap<AigNodeId, u64> = HashMap::new();
        values.insert(AigNodeId::FALSE, 0); // Constant false = all zeros
        for &input in &inputs {
            rng_state ^= rng_state << 13;
            rng_state ^= rng_state >> 7;
            rng_state ^= rng_state << 17;
            values.insert(input, rng_state);
        }
        if let Some(reset) = released {
            // The node value that makes the reset literal 0
            values.insert(reset.node, if reset.inverted { u64::MAX } else { 0 });
        }

        let orig_val = eval_lit(aig, original, &mut values);
        let recon_val = eval_lit(aig, reconstructed, &mut values);

        if orig_val != recon_val {
            if round == 0 {
                tracing::debug!("[DFF_VERIFY] latch {:?}: MISMATCH round {} orig={:016x} recon={:016x} xor={:016x}",
                    latch_id, round, orig_val, recon_val, orig_val ^ recon_val);
                tracing::debug!(
                    "[DFF_VERIFY]   original={:?} reconstructed={:?}",
                    original,
                    reconstructed
                );
            }
            return false;
        }
    }

    true
}

/// Collect all primary input and latch nodes in the transitive fanin of a node.
fn collect_inputs(
    aig: &Aig,
    node: AigNodeId,
    inputs: &mut HashSet<AigNodeId>,
    visited: &mut HashSet<AigNodeId>,
) {
    if node == AigNodeId::FALSE || !visited.insert(node) {
        return;
    }
    match aig.get_node(node) {
        Some(AigNode::And { left, right }) => {
            collect_inputs(aig, left.node, inputs, visited);
            collect_inputs(aig, right.node, inputs, visited);
        }
        _ => {
            // Input, Latch, or Barrier — treat as primary input for simulation
            inputs.insert(node);
        }
    }
}

/// Evaluate an AIG literal using 64-bit parallel simulation.
fn eval_lit(aig: &Aig, lit: AigLit, values: &mut HashMap<AigNodeId, u64>) -> u64 {
    let val = eval_node(aig, lit.node, values);
    if lit.inverted {
        !val
    } else {
        val
    }
}

fn eval_node(aig: &Aig, node: AigNodeId, values: &mut HashMap<AigNodeId, u64>) -> u64 {
    if let Some(&val) = values.get(&node) {
        return val;
    }
    let val = match aig.get_node(node) {
        Some(AigNode::And { left, right }) => {
            let left = *left;
            let right = *right;
            let l = eval_lit(aig, left, values);
            let r = eval_lit(aig, right, values);
            l & r
        }
        _ => 0, // Unknown node, treat as 0
    };
    values.insert(node, val);
    val
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
        assert!(
            decomp.enable.is_some(),
            "Should detect enable from nested MUX"
        );
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
        assert!(
            decomp.enable.is_some(),
            "Should detect enable with multiple data paths"
        );
    }

    /// Every assignment of `inputs` (and Q) gives the same value for the
    /// latch data and for `E ? D : Q`.
    fn assert_exact(aig: &Aig, latch: AigNodeId, data: AigLit, decomp: &LatchDecomp) {
        let mut inputs = HashSet::new();
        collect_inputs(aig, data.node, &mut inputs, &mut HashSet::new());
        inputs.insert(latch);
        let inputs: Vec<AigNodeId> = inputs.into_iter().collect();
        assert!(inputs.len() <= 6);
        // 64 patterns in parallel: bit k of input i's word is bit i of k
        let mut values = HashMap::new();
        values.insert(AigNodeId::FALSE, 0);
        for (i, &input) in inputs.iter().enumerate() {
            let word = (0..64u64).fold(0u64, |w, k| w | (((k >> i) & 1) << k));
            values.insert(input, word);
        }
        let mut v = values.clone();
        let f = eval_lit(aig, data, &mut v);
        let mut v = values.clone();
        let e = eval_lit(aig, decomp.enable.unwrap(), &mut v);
        let d = eval_lit(aig, decomp.data, &mut v);
        let q = values[&latch];
        let rebuilt = (e & d) | (!e & q);
        let used = if inputs.len() == 6 {
            u64::MAX
        } else {
            (1u64 << (1 << inputs.len())) - 1
        };
        assert_eq!(f & used, rebuilt & used, "F != E ? D : Q");
    }

    fn latch_data(aig: &Aig, latch: AigNodeId) -> AigLit {
        match aig.get_node(latch) {
            Some(AigNode::Latch { data, .. }) => *data,
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_reset_in_logic_folds_into_enable_and_data() {
        // F = !rst & (en ? d : Q), rst plain logic: it loads 0 whatever en says
        let mut aig = Aig::new("test".to_string());
        let rst = AigLit::new(aig.add_input("rst".to_string(), None));
        let en = AigLit::new(aig.add_input("en".to_string(), None));
        let d = AigLit::new(aig.add_input("d".to_string(), None));
        let q_node = aig.add_latch(AigLit::false_lit(), None, None, None);
        let q = AigLit::new(q_node);
        let hold = build_mux(&mut aig, en, d, q);
        let f = aig.add_and(rst.invert(), hold);
        aig.update_latch_data(q_node, f);

        let decomps = decompose_latches(&mut aig);
        let decomp = &decomps[&q_node];
        assert_exact(&aig, q_node, latch_data(&aig, q_node), decomp);
    }

    #[test]
    fn test_hold_in_both_branches() {
        // F = s ? (t ? a : Q) : (u ? Q : b)
        let mut aig = Aig::new("test".to_string());
        let [s, t, u, a, b] =
            ["s", "t", "u", "a", "b"].map(|n| AigLit::new(aig.add_input(n.to_string(), None)));
        let q_node = aig.add_latch(AigLit::false_lit(), None, None, None);
        let q = AigLit::new(q_node);
        let then_ = build_mux(&mut aig, t, a, q);
        let else_ = build_mux(&mut aig, u, q, b);
        let f = build_mux(&mut aig, s, then_, else_);
        aig.update_latch_data(q_node, f);

        let decomps = decompose_latches(&mut aig);
        let decomp = &decomps[&q_node];
        assert_exact(&aig, q_node, f, decomp);
    }

    #[test]
    fn test_enable_is_the_select_alone() {
        // F = en ? (a & b) : Q: the enable is en, the data does not read en
        let mut aig = Aig::new("test".to_string());
        let [en, a, b] = ["en", "a", "b"].map(|n| AigLit::new(aig.add_input(n.to_string(), None)));
        let q_node = aig.add_latch(AigLit::false_lit(), None, None, None);
        let ab = aig.add_and(a, b);
        let f = build_mux(&mut aig, en, ab, AigLit::new(q_node));
        aig.update_latch_data(q_node, f);

        let decomp = &decompose_latches(&mut aig)[&q_node];
        assert_eq!(decomp.enable, Some(en));
        assert_eq!(decomp.data, ab);
    }

    #[test]
    fn test_toggle_has_no_enable() {
        // F = t ? !Q : Q (a T flip-flop) passes Q and !Q: no hold form
        let mut aig = Aig::new("test".to_string());
        let t = AigLit::new(aig.add_input("t".to_string(), None));
        let q_node = aig.add_latch(AigLit::false_lit(), None, None, None);
        let q = AigLit::new(q_node);
        let f = build_mux(&mut aig, t, q.invert(), q);
        aig.update_latch_data(q_node, f);
        assert!(decompose_latches(&mut aig).is_empty());
    }

    #[test]
    fn test_select_reading_q_is_handled_as_logic() {
        // F = Q ? a : (en ? b : Q): the select reads Q itself
        let mut aig = Aig::new("test".to_string());
        let [a, b, en] = ["a", "b", "en"].map(|n| AigLit::new(aig.add_input(n.to_string(), None)));
        let q_node = aig.add_latch(AigLit::false_lit(), None, None, None);
        let q = AigLit::new(q_node);
        let inner = build_mux(&mut aig, en, b, q);
        let f = build_mux(&mut aig, q, a, inner);
        aig.update_latch_data(q_node, f);
        if let Some(decomp) = decompose_latches(&mut aig).get(&q_node) {
            assert_exact(&aig, q_node, f, decomp);
        }
    }

    /// A latch with reset `rst` whose data is `!rst & (en ? d : Q)`, as
    /// LIR lowering builds a register with a reset and an enable.
    fn reset_latch(sync: bool) -> (Aig, AigNodeId, [AigLit; 3]) {
        let mut aig = Aig::new("test".to_string());
        let [en, d] = ["en", "d"].map(|n| AigLit::new(aig.add_input(n.to_string(), None)));
        let rst = aig.add_input("rst".to_string(), None);
        let q_node = aig.add_latch(
            AigLit::false_lit(),
            Some(false),
            None,
            Some(AigLit::new(rst)),
        );
        if sync {
            aig.mark_latch_sync_reset(q_node);
        }
        let hold = build_mux(&mut aig, en, d, AigLit::new(q_node));
        let f = aig.add_and(AigLit::new(rst).invert(), hold);
        aig.update_latch_data(q_node, f);
        (aig, q_node, [en, d, AigLit::new(rst)])
    }

    #[test]
    fn test_async_reset_leaves_enable_and_data_free_of_it() {
        // The reset pin overrides the enable, so the cell needs E and D only
        // with the reset released
        let (mut aig, q_node, [en, d, _]) = reset_latch(false);
        let decomp = &decompose_latches(&mut aig)[&q_node];
        assert_eq!(decomp.enable, Some(en));
        assert_eq!(decomp.data, d);
    }

    #[test]
    fn test_async_reset_in_a_select_picks_the_released_branch() {
        // F = rst ? 0 : (en ? d : Q)
        let mut aig = Aig::new("test".to_string());
        let [en, d] = ["en", "d"].map(|n| AigLit::new(aig.add_input(n.to_string(), None)));
        let rst = aig.add_input("rst".to_string(), None);
        let q_node = aig.add_latch(
            AigLit::false_lit(),
            Some(false),
            None,
            Some(AigLit::new(rst)),
        );
        let hold = build_mux(&mut aig, en, d, AigLit::new(q_node));
        let f = build_mux(&mut aig, AigLit::new(rst), AigLit::true_lit(), hold);
        aig.update_latch_data(q_node, f);
        let decomp = &decompose_latches(&mut aig)[&q_node];
        assert_eq!(decomp.enable, Some(en));
        assert_eq!(decomp.data, d);
    }

    #[test]
    fn test_sync_reset_loads_through_enable_and_data() {
        // No reset pin: E = rst | en, D = rst ? 0 : d
        let (mut aig, q_node, [en, d, rst]) = reset_latch(true);
        let decomp = decompose_latches(&mut aig)[&q_node].clone();
        let mut inputs = HashMap::new();
        inputs.insert(AigNodeId::FALSE, 0u64);
        for (i, lit) in [en, d, rst, AigLit::new(q_node)].iter().enumerate() {
            let word = (0..64u64).fold(0u64, |w, k| w | (((k >> i) & 1) << k));
            inputs.insert(lit.node, word);
        }
        let mut v = inputs.clone();
        let e = eval_lit(&aig, decomp.enable.unwrap(), &mut v);
        let dd = eval_lit(&aig, decomp.data, &mut v);
        let [en, d, rst, q] = [en, d, rst, AigLit::new(q_node)].map(|l| inputs[&l.node]);
        let expected = !rst & ((en & d) | (!en & q));
        assert_eq!((e & dd) | (!e & q), expected);
        assert_ne!(e, en, "the reset must load even when en is low");
    }
}
