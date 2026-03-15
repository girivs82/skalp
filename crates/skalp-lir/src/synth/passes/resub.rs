//! AIG Resubstitution Pass
//!
//! This pass performs Boolean resubstitution using simulation-based divisor matching.
//! For each node, it checks whether the node's function can be re-expressed using
//! existing nodes (divisors), potentially saving AND gates.
//!
//! # Algorithm
//!
//! 1. Compute 64-bit simulation signatures for all nodes using random input patterns
//! 2. For each AND node (reverse topological order for maximum MFFC):
//!    a. Compute MFFC size using reference counting
//!    b. Collect divisor candidates (nodes not in the MFFC)
//!    c. Use simulation to check resubstitution patterns:
//!       - 0-resub: target == divisor (saves MFFC nodes)
//!       - 1-resub: target == d1 AND/OR/XOR d2 (saves MFFC-1 nodes)
//!       - 2-resub: target == (d1 AND d2) OR d3 / MUX (saves MFFC-2 nodes)
//!    d. Apply the first profitable match
//!
//! # References
//!
//! - Mishchenko, A., Brayton, R., Jiang, J.-H. R., & Jang, S. (2011).
//!   Scalable don't-care-based logic optimization and resynthesis.

use super::{Pass, PassResult};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId};
use indexmap::IndexMap;

/// Maximum number of divisors to consider per target node
const MAX_DIVISORS: usize = 150;

/// Resubstitution pass
pub struct Resub {
    /// Number of nodes resubstituted
    resub_count: usize,
    /// Total nodes saved
    total_savings: i32,
    /// Zero-cost mode
    zero_cost: bool,
}

impl Resub {
    pub fn new() -> Self {
        Self {
            resub_count: 0,
            total_savings: 0,
            zero_cost: false,
        }
    }

    pub fn zero_cost() -> Self {
        Self {
            resub_count: 0,
            total_savings: 0,
            zero_cost: true,
        }
    }

    pub fn with_params(_max_divisors: usize, _max_inputs: usize) -> Self {
        Self::new()
    }
}

impl Default for Resub {
    fn default() -> Self {
        Self::new()
    }
}

/// Number of independent 64-bit simulation rounds.
/// Deep AND chains (e.g., carry chains) can produce zero signatures with probability
/// (1 - 2^-N)^64 per round. With 8 rounds, false positive probability drops to
/// ~10^-9 even for wide carry chains.
const SIM_ROUNDS: usize = 8;

/// Multi-round 64-bit simulation signatures for all nodes.
/// Uses multiple independent random seeds to avoid false positives from
/// deep AND chains producing all-zero signatures.
struct SimSignatures {
    rounds: Vec<IndexMap<AigNodeId, u64>>,
}

impl SimSignatures {
    /// Compute simulation signatures bottom-up using random input patterns
    fn compute(aig: &Aig) -> Self {
        let seeds: [u64; SIM_ROUNDS] = [
            0x12345678_DEADBEEF,
            0xA5A5A5A5_5A5A5A5A,
            0x0F0F0F0F_F0F0F0F0,
            0x13579BDF_2468ACE0,
            0xFEDCBA98_76543210,
            0x0123CDEF_89AB4567,
            0xDEADBEEF_CAFEBABE,
            0x8BADF00D_DEADC0DE,
        ];

        let mut rounds = Vec::with_capacity(SIM_ROUNDS);

        for &seed in &seeds[..SIM_ROUNDS] {
            let mut sigs = IndexMap::new();
            sigs.insert(AigNodeId::FALSE, 0u64);

            let mut rng_state: u64 = seed;
            for (id, node) in aig.iter_nodes() {
                match node {
                    AigNode::Input { .. } | AigNode::Latch { .. } | AigNode::Barrier { .. } => {
                        rng_state = rng_state.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
                        sigs.insert(id, rng_state);
                    }
                    _ => {}
                }
            }

            for (id, node) in aig.iter_nodes() {
                if let AigNode::And { left, right } = node {
                    let left_sig = sigs.get(&left.node).copied().unwrap_or(0);
                    let left_sig = if left.inverted { !left_sig } else { left_sig };
                    let right_sig = sigs.get(&right.node).copied().unwrap_or(0);
                    let right_sig = if right.inverted { !right_sig } else { right_sig };
                    sigs.insert(id, left_sig & right_sig);
                }
            }

            rounds.push(sigs);
        }

        Self { rounds }
    }

    /// Get signature for round 0 (used for primary matching)
    fn get(&self, node: AigNodeId) -> u64 {
        self.rounds[0].get(&node).copied().unwrap_or(0)
    }

    /// Get signature for a specific round
    fn get_round(&self, round: usize, node: AigNodeId) -> u64 {
        self.rounds[round].get(&node).copied().unwrap_or(0)
    }
}

/// Count MFFC nodes by decrementing reference counts.
/// Returns the number of AND nodes in the MFFC.
/// WARNING: Mutates ref_counts. Call mffc_ref to restore.
fn mffc_deref(
    aig: &Aig,
    root: AigNodeId,
    ref_counts: &mut IndexMap<AigNodeId, usize>,
) -> usize {
    let (left, right) = match aig.get_node(root) {
        Some(AigNode::And { left, right }) => (*left, *right),
        _ => return 0,
    };

    let mut count = 1;

    let left_rc = ref_counts.get(&left.node).copied().unwrap_or(0);
    if left_rc > 0 {
        *ref_counts.entry(left.node).or_insert(0) = left_rc - 1;
        if left_rc == 1 {
            count += mffc_deref(aig, left.node, ref_counts);
        }
    }

    let right_rc = ref_counts.get(&right.node).copied().unwrap_or(0);
    if right_rc > 0 {
        *ref_counts.entry(right.node).or_insert(0) = right_rc - 1;
        if right_rc == 1 {
            count += mffc_deref(aig, right.node, ref_counts);
        }
    }

    count
}

/// Restore reference counts after mffc_deref.
fn mffc_ref(
    aig: &Aig,
    root: AigNodeId,
    ref_counts: &mut IndexMap<AigNodeId, usize>,
) -> usize {
    let (left, right) = match aig.get_node(root) {
        Some(AigNode::And { left, right }) => (*left, *right),
        _ => return 0,
    };

    let mut count = 1;

    let left_rc = ref_counts.get(&left.node).copied().unwrap_or(0);
    if left_rc == 0 {
        count += mffc_ref(aig, left.node, ref_counts);
    }
    *ref_counts.entry(left.node).or_insert(0) = left_rc + 1;

    let right_rc = ref_counts.get(&right.node).copied().unwrap_or(0);
    if right_rc == 0 {
        count += mffc_ref(aig, right.node, ref_counts);
    }
    *ref_counts.entry(right.node).or_insert(0) = right_rc + 1;

    count
}

/// Collect nodes in the MFFC (nodes that become dead when root is removed)
fn collect_mffc_nodes(
    aig: &Aig,
    root: AigNodeId,
    ref_counts: &IndexMap<AigNodeId, usize>,
) -> Vec<AigNodeId> {
    let mut mffc = Vec::new();
    let mut temp_refs = ref_counts.clone();
    collect_mffc_recursive(aig, root, &mut temp_refs, &mut mffc);
    mffc
}

fn collect_mffc_recursive(
    aig: &Aig,
    node: AigNodeId,
    ref_counts: &mut IndexMap<AigNodeId, usize>,
    mffc: &mut Vec<AigNodeId>,
) {
    let (left, right) = match aig.get_node(node) {
        Some(AigNode::And { left, right }) => (*left, *right),
        _ => return,
    };

    mffc.push(node);

    let left_rc = ref_counts.get(&left.node).copied().unwrap_or(0);
    if left_rc > 0 {
        *ref_counts.entry(left.node).or_insert(0) = left_rc - 1;
        if left_rc == 1 {
            collect_mffc_recursive(aig, left.node, ref_counts, mffc);
        }
    }

    let right_rc = ref_counts.get(&right.node).copied().unwrap_or(0);
    if right_rc > 0 {
        *ref_counts.entry(right.node).or_insert(0) = right_rc - 1;
        if right_rc == 1 {
            collect_mffc_recursive(aig, right.node, ref_counts, mffc);
        }
    }
}

/// Compute fanout counts
fn compute_fanout_counts(aig: &Aig) -> IndexMap<AigNodeId, usize> {
    let mut counts = IndexMap::new();

    for (_, node) in aig.iter_nodes() {
        for fanin in node.fanins() {
            *counts.entry(fanin.node).or_insert(0) += 1;
        }
    }

    for (_, lit) in aig.outputs() {
        *counts.entry(lit.node).or_insert(0) += 1;
    }

    counts
}

/// Collect divisor candidates for a target node.
/// Divisors are nodes in the combinational transitive fanin that are NOT in the MFFC.
/// Important: does NOT cross latch/barrier boundaries to avoid collecting nodes
/// from the sequential feedback path (which could create combinational cycles).
fn collect_divisors(
    aig: &Aig,
    target: AigNodeId,
    mffc_nodes: &[AigNodeId],
    fanout_counts: &IndexMap<AigNodeId, usize>,
) -> Vec<AigNodeId> {
    let mffc_set: std::collections::HashSet<AigNodeId> = mffc_nodes.iter().copied().collect();
    let mut divisors = Vec::new();
    let mut visited = std::collections::HashSet::new();
    let mut stack = vec![target];

    // Walk the combinational transitive fanin, collecting non-MFFC AND/Input nodes.
    // Stop at latches and barriers — they represent sequential boundaries.
    while let Some(node) = stack.pop() {
        if visited.contains(&node) || divisors.len() >= MAX_DIVISORS {
            continue;
        }
        visited.insert(node);

        match aig.get_node(node) {
            Some(AigNode::And { left, right }) => {
                if node != target && !mffc_set.contains(&node) {
                    divisors.push(node);
                }
                stack.push(left.node);
                stack.push(right.node);
            }
            Some(AigNode::Input { .. }) => {
                if node != target {
                    divisors.push(node);
                }
            }
            Some(AigNode::Latch { .. } | AigNode::Barrier { .. }) => {
                // Latch/barrier outputs are valid divisors (they act as pseudo-inputs
                // for the combinational cone), but don't follow their data inputs
                if node != target {
                    divisors.push(node);
                }
            }
            _ => {}
        }
    }

    // Sort by fanout (prefer high-fanout divisors — more likely to be useful)
    divisors.sort_by(|&a, &b| {
        let fa = fanout_counts.get(&a).copied().unwrap_or(0);
        let fb = fanout_counts.get(&b).copied().unwrap_or(0);
        fb.cmp(&fa)
    });

    divisors.truncate(MAX_DIVISORS);
    divisors
}

/// Result of a resubstitution match
enum ResubMatch {
    /// target = lit (0 new gates)
    Equal(AigLit),
    /// target = d1 AND d2 (1 new gate)
    And2(AigLit, AigLit),
    /// target = !(d1 AND d2) = d1 NAND d2 (1 new gate, result inverted)
    Or2(AigLit, AigLit),
    /// target = d1 XOR d2 (3 new gates in AIG)
    Xor2(AigLit, AigLit),
    /// target = (d1 AND d2) OR d3 (2 new gates)
    AndOr(AigLit, AigLit, AigLit),
    /// target = (d1 OR d2) AND d3 (2 new gates)
    OrAnd(AigLit, AigLit, AigLit),
    /// target = sel ? d1 : d2 = (sel AND d1) OR (!sel AND d2) (3 new gates)
    Mux(AigLit, AigLit, AigLit),
}

impl ResubMatch {
    /// Number of new AND gates this pattern requires
    fn cost(&self) -> usize {
        match self {
            ResubMatch::Equal(_) => 0,
            ResubMatch::And2(_, _) | ResubMatch::Or2(_, _) => 1,
            ResubMatch::AndOr(_, _, _) | ResubMatch::OrAnd(_, _, _) => 2,
            ResubMatch::Xor2(_, _) | ResubMatch::Mux(_, _, _) => 3,
        }
    }

    /// Build the resubstitution in the AIG
    fn build(&self, aig: &mut Aig) -> AigLit {
        match self {
            ResubMatch::Equal(lit) => *lit,
            ResubMatch::And2(a, b) => aig.add_and(*a, *b),
            ResubMatch::Or2(a, b) => {
                // OR = NAND of complements: !((!a) & (!b))
                let nand = aig.add_and(a.invert(), b.invert());
                nand.invert()
            }
            ResubMatch::Xor2(a, b) => {
                // XOR = (a & !b) | (!a & b) = !(!(a & !b) & !(!a & b))
                let a_and_nb = aig.add_and(*a, b.invert());
                let na_and_b = aig.add_and(a.invert(), *b);
                let nand = aig.add_and(a_and_nb.invert(), na_and_b.invert());
                nand.invert()
            }
            ResubMatch::AndOr(a, b, c) => {
                // (a AND b) OR c = !(!( a AND b) AND !c)
                let ab = aig.add_and(*a, *b);
                let nand = aig.add_and(ab.invert(), c.invert());
                nand.invert()
            }
            ResubMatch::OrAnd(a, b, c) => {
                // (a OR b) AND c = !(!a & !b) & c = !((!a & !b)) & c
                let nor = aig.add_and(a.invert(), b.invert());
                aig.add_and(nor.invert(), *c)
            }
            ResubMatch::Mux(sel, d1, d0) => {
                // MUX: sel ? d1 : d0 = (sel & d1) | (!sel & d0)
                let sel_d1 = aig.add_and(*sel, *d1);
                let nsel_d0 = aig.add_and(sel.invert(), *d0);
                let nand = aig.add_and(sel_d1.invert(), nsel_d0.invert());
                nand.invert()
            }
        }
    }
}

/// Verify a candidate resub match against additional simulation rounds.
/// Returns true if the match holds for ALL rounds (not just round 0).
fn verify_resub_multi_round(
    sigs: &SimSignatures,
    target: AigNodeId,
    resub: &ResubMatch,
) -> bool {
    for round in 1..SIM_ROUNDS {
        let target_sig = sigs.get_round(round, target);
        let ok = match resub {
            ResubMatch::Equal(a) => {
                let sa = sigs.get_round(round, a.node);
                let sa = if a.inverted { !sa } else { sa };
                target_sig == sa
            }
            ResubMatch::And2(a, b) => {
                let sa = if a.inverted { !sigs.get_round(round, a.node) } else { sigs.get_round(round, a.node) };
                let sb = if b.inverted { !sigs.get_round(round, b.node) } else { sigs.get_round(round, b.node) };
                target_sig == (sa & sb)
            }
            ResubMatch::Or2(a, b) => {
                let sa = if a.inverted { !sigs.get_round(round, a.node) } else { sigs.get_round(round, a.node) };
                let sb = if b.inverted { !sigs.get_round(round, b.node) } else { sigs.get_round(round, b.node) };
                target_sig == (sa | sb)
            }
            ResubMatch::Xor2(a, b) => {
                let sa = if a.inverted { !sigs.get_round(round, a.node) } else { sigs.get_round(round, a.node) };
                let sb = if b.inverted { !sigs.get_round(round, b.node) } else { sigs.get_round(round, b.node) };
                target_sig == (sa ^ sb)
            }
            ResubMatch::AndOr(a, b, c) => {
                let sa = if a.inverted { !sigs.get_round(round, a.node) } else { sigs.get_round(round, a.node) };
                let sb = if b.inverted { !sigs.get_round(round, b.node) } else { sigs.get_round(round, b.node) };
                let sc = if c.inverted { !sigs.get_round(round, c.node) } else { sigs.get_round(round, c.node) };
                target_sig == ((sa & sb) | sc)
            }
            ResubMatch::OrAnd(a, b, c) => {
                let sa = if a.inverted { !sigs.get_round(round, a.node) } else { sigs.get_round(round, a.node) };
                let sb = if b.inverted { !sigs.get_round(round, b.node) } else { sigs.get_round(round, b.node) };
                let sc = if c.inverted { !sigs.get_round(round, c.node) } else { sigs.get_round(round, c.node) };
                target_sig == ((sa | sb) & sc)
            }
            ResubMatch::Mux(sel, d1, d0) => {
                let ss = if sel.inverted { !sigs.get_round(round, sel.node) } else { sigs.get_round(round, sel.node) };
                let s1 = if d1.inverted { !sigs.get_round(round, d1.node) } else { sigs.get_round(round, d1.node) };
                let s0 = if d0.inverted { !sigs.get_round(round, d0.node) } else { sigs.get_round(round, d0.node) };
                target_sig == ((ss & s1) | (!ss & s0))
            }
        };
        if !ok {
            return false;
        }
    }
    true
}

/// Try to find a resubstitution for target using simulation signatures.
/// Returns the best match (lowest cost) if any.
fn try_resub_sim(
    target_sig: u64,
    divisors: &[(AigNodeId, u64)],
    mffc_size: usize,
    zero_cost: bool,
) -> Option<ResubMatch> {
    let min_gain = if zero_cost { 0 } else { 1 };

    // 0-resub: target == divisor or target == !divisor
    // Cost = 0, gain = mffc_size
    if mffc_size as i32 >= min_gain as i32 {
        for &(div, div_sig) in divisors {
            if target_sig == div_sig {
                return Some(ResubMatch::Equal(AigLit::new(div)));
            }
            if target_sig == !div_sig {
                return Some(ResubMatch::Equal(AigLit::not(div)));
            }
        }
    }

    // 1-resub AND/OR: cost = 1, gain = mffc_size - 1
    if mffc_size as i32 - 1 >= min_gain as i32 {
        let n = divisors.len().min(50);
        for i in 0..n {
            let (d1, s1) = divisors[i];
            for j in (i + 1)..n {
                let (d2, s2) = divisors[j];

                // AND variants
                if target_sig == s1 & s2 {
                    return Some(ResubMatch::And2(AigLit::new(d1), AigLit::new(d2)));
                }
                if target_sig == s1 & !s2 {
                    return Some(ResubMatch::And2(AigLit::new(d1), AigLit::not(d2)));
                }
                if target_sig == !s1 & s2 {
                    return Some(ResubMatch::And2(AigLit::not(d1), AigLit::new(d2)));
                }
                if target_sig == !s1 & !s2 {
                    return Some(ResubMatch::And2(AigLit::not(d1), AigLit::not(d2)));
                }

                // OR variants
                if target_sig == (s1 | s2) {
                    return Some(ResubMatch::Or2(AigLit::new(d1), AigLit::new(d2)));
                }
                if target_sig == (s1 | !s2) {
                    return Some(ResubMatch::Or2(AigLit::new(d1), AigLit::not(d2)));
                }
                if target_sig == (!s1 | s2) {
                    return Some(ResubMatch::Or2(AigLit::not(d1), AigLit::new(d2)));
                }
                if target_sig == (!s1 | !s2) {
                    return Some(ResubMatch::Or2(AigLit::not(d1), AigLit::not(d2)));
                }

                // XOR variants (cost = 3, need mffc >= 4 for positive gain)
                if mffc_size as i32 - 3 >= min_gain as i32 {
                    if target_sig == (s1 ^ s2) {
                        return Some(ResubMatch::Xor2(AigLit::new(d1), AigLit::new(d2)));
                    }
                }
            }
        }
    }

    // 2-resub AND-OR / OR-AND: cost = 2, gain = mffc_size - 2
    if mffc_size as i32 - 2 >= min_gain as i32 {
        let n = divisors.len().min(30);
        for i in 0..n {
            let (d1, s1) = divisors[i];
            for j in (i + 1)..n {
                let (d2, s2) = divisors[j];
                let and_12 = s1 & s2;
                let or_12 = s1 | s2;

                for k in 0..n {
                    if k == i || k == j {
                        continue;
                    }
                    let (d3, s3) = divisors[k];

                    // (d1 AND d2) OR d3
                    if target_sig == (and_12 | s3) {
                        return Some(ResubMatch::AndOr(
                            AigLit::new(d1), AigLit::new(d2), AigLit::new(d3),
                        ));
                    }
                    if target_sig == (and_12 | !s3) {
                        return Some(ResubMatch::AndOr(
                            AigLit::new(d1), AigLit::new(d2), AigLit::not(d3),
                        ));
                    }

                    // (!d1 AND d2) OR d3
                    if target_sig == ((!s1 & s2) | s3) {
                        return Some(ResubMatch::AndOr(
                            AigLit::not(d1), AigLit::new(d2), AigLit::new(d3),
                        ));
                    }

                    // (d1 AND !d2) OR d3
                    if target_sig == ((s1 & !s2) | s3) {
                        return Some(ResubMatch::AndOr(
                            AigLit::new(d1), AigLit::not(d2), AigLit::new(d3),
                        ));
                    }

                    // (d1 OR d2) AND d3
                    if target_sig == (or_12 & s3) {
                        return Some(ResubMatch::OrAnd(
                            AigLit::new(d1), AigLit::new(d2), AigLit::new(d3),
                        ));
                    }

                    // MUX: sel ? d2 : d3 (cost = 3, need mffc >= 4)
                    if mffc_size as i32 - 3 >= min_gain as i32 {
                        if target_sig == ((s1 & s2) | (!s1 & s3)) {
                            return Some(ResubMatch::Mux(
                                AigLit::new(d1), AigLit::new(d2), AigLit::new(d3),
                            ));
                        }
                    }
                }
            }
        }
    }

    None
}

impl Pass for Resub {
    fn name(&self) -> &str {
        if self.zero_cost { "resub_z" } else { "resub" }
    }

    fn run(&mut self, aig: &mut Aig) -> PassResult {
        let mut result = PassResult::new(self.name());
        result.record_before(aig);

        self.resub_count = 0;
        self.total_savings = 0;

        // Step 1: Compute simulation signatures
        let sigs = SimSignatures::compute(aig);

        // Step 2: Compute reference counts
        let mut ref_counts = compute_fanout_counts(aig);

        // Step 3: Collect AND nodes in topological order (forward)
        // Processing inputs-first ensures substituted nodes' new literals
        // don't create forward references during rebuild.
        let topo_nodes: Vec<AigNodeId> = aig
            .iter_nodes()
            .filter_map(|(id, node)| {
                if matches!(node, AigNode::And { .. }) {
                    Some(id)
                } else {
                    None
                }
            })
            .collect();

        let fanout_counts = ref_counts.clone();

        // Collect resub matches (target → pattern), then apply one at a time.
        // We collect matches first (without building) so the AIG doesn't change
        // during the scan, then build and apply each substitution individually.
        struct ResubEntry {
            target: AigNodeId,
            divisors: Vec<(AigNodeId, bool)>, // (node, inverted) pairs from match
            pattern_kind: u8,                  // 0=Equal, 1=And2, 2=Or2, 3=Xor2, 4=AndOr, 5=OrAnd, 6=Mux
            gain: i32,
        }

        let mut entries: Vec<ResubEntry> = Vec::new();
        let mut substituted_nodes: std::collections::HashSet<AigNodeId> = std::collections::HashSet::new();

        for target in &topo_nodes {
            if substituted_nodes.contains(target) {
                continue;
            }

            // Compute MFFC size
            let mffc_size = mffc_deref(aig, *target, &mut ref_counts);
            mffc_ref(aig, *target, &mut ref_counts);

            if mffc_size == 0 {
                continue;
            }

            let mffc_nodes = collect_mffc_nodes(aig, *target, &fanout_counts);
            if mffc_nodes.iter().any(|n| substituted_nodes.contains(n)) {
                continue;
            }

            let divisors_ids = collect_divisors(aig, *target, &mffc_nodes, &fanout_counts);
            let divisor_sigs: Vec<(AigNodeId, u64)> = divisors_ids
                .iter()
                .filter(|&&d| !substituted_nodes.contains(&d))
                .map(|&d| (d, sigs.get(d)))
                .collect();

            let target_sig = sigs.get(*target);

            if let Some(resub_match) = try_resub_sim(target_sig, &divisor_sigs, mffc_size, self.zero_cost) {
                let gain = mffc_size as i32 - resub_match.cost() as i32;
                let accept = if self.zero_cost { gain >= 0 } else { gain > 0 };

                // Verify against additional simulation rounds to catch false positives
                // (e.g., deep AND chains producing all-zero signatures)
                if accept && verify_resub_multi_round(&sigs, *target, &resub_match) {
                    // Extract divisor info from the match (without building)
                    let (kind, divs) = match &resub_match {
                        ResubMatch::Equal(a) => (0u8, vec![(a.node, a.inverted)]),
                        ResubMatch::And2(a, b) => (1, vec![(a.node, a.inverted), (b.node, b.inverted)]),
                        ResubMatch::Or2(a, b) => (2, vec![(a.node, a.inverted), (b.node, b.inverted)]),
                        ResubMatch::Xor2(a, b) => (3, vec![(a.node, a.inverted), (b.node, b.inverted)]),
                        ResubMatch::AndOr(a, b, c) => (4, vec![(a.node, a.inverted), (b.node, b.inverted), (c.node, c.inverted)]),
                        ResubMatch::OrAnd(a, b, c) => (5, vec![(a.node, a.inverted), (b.node, b.inverted), (c.node, c.inverted)]),
                        ResubMatch::Mux(a, b, c) => (6, vec![(a.node, a.inverted), (b.node, b.inverted), (c.node, c.inverted)]),
                    };

                    substituted_nodes.insert(*target);
                    for &mffc_node in &mffc_nodes {
                        substituted_nodes.insert(mffc_node);
                    }

                    entries.push(ResubEntry {
                        target: *target,
                        divisors: divs,
                        pattern_kind: kind,
                        gain,
                    });
                }
            }
        }

        // Build and apply all substitutions in a single batch.
        // Clear strash before building to prevent structural hashing from returning
        // existing nodes that contain targets as children (which would create
        // self-referencing cycles after apply_substitutions).
        if !entries.is_empty() {
            aig.clear_strash();
            let mut subst_map: IndexMap<AigNodeId, AigLit> = IndexMap::new();

            for entry in &entries {
                let lits: Vec<AigLit> = entry.divisors.iter()
                    .map(|&(node, inv)| AigLit { node, inverted: inv })
                    .collect();

                let new_lit = match entry.pattern_kind {
                    0 => lits[0], // Equal
                    1 => aig.add_and(lits[0], lits[1]), // And2
                    2 => { // Or2
                        let nand = aig.add_and(lits[0].invert(), lits[1].invert());
                        nand.invert()
                    }
                    3 => { // Xor2
                        let a_nb = aig.add_and(lits[0], lits[1].invert());
                        let na_b = aig.add_and(lits[0].invert(), lits[1]);
                        let nand = aig.add_and(a_nb.invert(), na_b.invert());
                        nand.invert()
                    }
                    4 => { // AndOr
                        let ab = aig.add_and(lits[0], lits[1]);
                        let nand = aig.add_and(ab.invert(), lits[2].invert());
                        nand.invert()
                    }
                    5 => { // OrAnd
                        let nor = aig.add_and(lits[0].invert(), lits[1].invert());
                        aig.add_and(nor.invert(), lits[2])
                    }
                    6 => { // Mux
                        let sel_d1 = aig.add_and(lits[0], lits[1]);
                        let nsel_d0 = aig.add_and(lits[0].invert(), lits[2]);
                        let nand = aig.add_and(sel_d1.invert(), nsel_d0.invert());
                        nand.invert()
                    }
                    _ => unreachable!(),
                };

                if new_lit.node != entry.target {
                    // Safety check: verify the replacement doesn't transitively
                    // reference the target (would create a cycle after substitution)
                    let creates_cycle = {
                        let mut stack = vec![new_lit.node];
                        let mut visited = std::collections::HashSet::new();
                        let mut found = false;
                        while let Some(n) = stack.pop() {
                            if n == entry.target { found = true; break; }
                            if !visited.insert(n) { continue; }
                            if let Some(AigNode::And { left, right }) = aig.get_node(n) {
                                stack.push(left.node);
                                stack.push(right.node);
                            }
                        }
                        found
                    };
                    if creates_cycle {
                        continue;
                    }
                    subst_map.insert(entry.target, new_lit);
                    self.resub_count += 1;
                    self.total_savings += entry.gain;
                }
            }

            if !subst_map.is_empty() {
                aig.apply_substitutions(&subst_map);
                super::rebuild_aig_topological(aig);
            }
        }

        result.record_after(aig);
        result.add_extra("nodes_resubstituted", &self.resub_count.to_string());
        result.add_extra("nodes_saved", &self.total_savings.to_string());
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resub_creation() {
        let pass = Resub::new();
        assert_eq!(pass.name(), "resub");

        let pass_z = Resub::zero_cost();
        assert_eq!(pass_z.name(), "resub_z");
    }

    #[test]
    fn test_resub_simple() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let mut pass = Resub::new();
        let result = pass.run(&mut aig);

        assert!(result.ands_after >= 1);
    }

    #[test]
    fn test_simulation_signatures() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let sigs = SimSignatures::compute(&aig);
        let sig_a = sigs.get(a);
        let sig_b = sigs.get(b);
        let sig_ab = sigs.get(ab.node);

        // AND node should have signature = sig_a & sig_b
        assert_eq!(sig_ab, sig_a & sig_b);
    }

    #[test]
    fn test_resub_finds_equal() {
        // Create circuit where one node equals another
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        // ab = a & b
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        // abc = ab & c
        let abc = aig.add_and(ab, AigLit::new(c));
        // ab2 = a & b (duplicate — should be caught by resub)
        // Note: strash normally catches this, but let's test resub
        let ab2 = aig.add_and(AigLit::new(a), AigLit::new(b));
        let result_node = aig.add_and(abc, ab2);
        aig.add_output("y".to_string(), result_node);

        let before = aig.and_count();
        let mut pass = Resub::new();
        pass.run(&mut aig);

        // Structural hashing in add_and should have already merged ab2 with ab,
        // but if it didn't, resub should find it
        assert!(aig.and_count() <= before);
    }

    #[test]
    fn test_collect_divisors() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        aig.add_output("y".to_string(), abc);

        let fanout_counts = compute_fanout_counts(&aig);
        let mffc_nodes = collect_mffc_nodes(&aig, abc.node, &fanout_counts);
        let divisors = collect_divisors(&aig, abc.node, &mffc_nodes, &fanout_counts);

        // Should include some fanin nodes (a, b, c, possibly ab)
        assert!(!divisors.is_empty());
    }

    #[test]
    fn test_with_params() {
        let pass = Resub::with_params(100, 5);
        assert_eq!(pass.name(), "resub");
    }

    #[test]
    fn test_mffc_size() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        // Linear chain: ab = a & b, abc = ab & c
        // ab has fanout 1 (only abc uses it)
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        aig.add_output("y".to_string(), abc);

        let mut ref_counts = compute_fanout_counts(&aig);
        let mffc = mffc_deref(&aig, abc.node, &mut ref_counts);
        mffc_ref(&aig, abc.node, &mut ref_counts);

        // MFFC of abc includes both abc and ab (ab has single fanout)
        assert_eq!(mffc, 2);
    }
}
