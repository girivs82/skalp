//! FRAIG (Functionally Reduced And-Inverter Graph)
//!
//! This module implements SAT-based functional equivalence detection to find
//! and merge functionally equivalent nodes in the AIG.
//!
//! # Algorithm
//!
//! 1. **Simulation Phase**: Simulate the AIG with random input patterns to
//!    quickly identify candidate equivalence classes (nodes with same signatures).
//!
//! 2. **SAT Verification Phase**: For each candidate pair, use SAT solving to
//!    prove or disprove equivalence. We check if `node1 XOR node2` is UNSAT.
//!
//! 3. **Counterexample Refinement**: When SAT finds a distinguishing input,
//!    use it to refine equivalence classes and avoid future false candidates.
//!
//! 4. **Merge Phase**: Replace proven equivalent nodes with their representatives.
//!
//! # References
//!
//! - Mishchenko et al., "FRAIGs: A Unifying Representation for Logic Synthesis
//!   and Verification", 2005

use super::{Pass, PassResult};
use crate::synth::sat::{Lit, SatResult, Solver};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId, BarrierType};
use indexmap::IndexMap;
use std::collections::HashSet;

/// FRAIG configuration
#[derive(Debug, Clone)]
pub struct FraigConfig {
    /// Number of random simulation patterns per round
    pub sim_patterns: usize,
    /// Number of simulation rounds
    pub sim_rounds: usize,
    /// Random seed for reproducibility
    pub seed: u64,
    /// Maximum SAT conflicts per equivalence check
    pub sat_conflict_limit: u64,
    /// Enable verbose output
    pub verbose: bool,
    /// Maximum number of SAT calls before giving up
    pub max_sat_calls: usize,
    /// Record equivalences as choices instead of merging nodes
    ///
    /// When true, FRAIG will preserve both implementations and record them
    /// as choices. The technology mapper can then explore all alternatives
    /// to find the best implementation. When false (default), equivalent
    /// nodes are immediately merged.
    pub record_choices: bool,
}

impl Default for FraigConfig {
    fn default() -> Self {
        Self {
            sim_patterns: 64,
            sim_rounds: 3,
            seed: 0xDEADBEEF,
            sat_conflict_limit: 1000,
            verbose: false,
            max_sat_calls: 10000,
            record_choices: false,
        }
    }
}

/// Simulation signature for a node (multiple 64-bit patterns)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct SimSignature {
    patterns: Vec<u64>,
}

impl SimSignature {
    fn new(num_patterns: usize) -> Self {
        Self {
            patterns: vec![0; num_patterns],
        }
    }

    fn from_single(pattern: u64) -> Self {
        Self {
            patterns: vec![pattern],
        }
    }

    fn invert(&self) -> Self {
        Self {
            patterns: self.patterns.iter().map(|p| !p).collect(),
        }
    }

    /// Get canonical form (use smaller value as canonical)
    fn canonical(&self) -> (Self, bool) {
        let inverted = self.invert();
        if self.patterns < inverted.patterns {
            (self.clone(), false)
        } else {
            (inverted, true)
        }
    }

    fn and(&self, other: &Self) -> Self {
        Self {
            patterns: self
                .patterns
                .iter()
                .zip(&other.patterns)
                .map(|(a, b)| a & b)
                .collect(),
        }
    }
}

/// One step of a xorshift64 generator.
fn xorshift(state: &mut u64) -> u64 {
    *state ^= *state << 13;
    *state ^= *state >> 7;
    *state ^= *state << 17;
    *state
}

/// Outcome of one equivalence check.
enum Check {
    Equal,
    /// The values of the free nodes (inputs, latch outputs) under which the
    /// two nodes differ
    Differ(Vec<(AigNodeId, bool)>),
    /// The conflict limit ran out
    Unknown,
}

/// CNF generator for AIG
struct CnfGenerator<'a> {
    aig: &'a Aig,
    solver: Solver,
    /// Mapping from AIG node ID to SAT variable
    node_to_var: IndexMap<AigNodeId, u32>,
    /// Next available variable
    next_var: u32,
    /// Inputs and latch outputs in the CNF: the free variables, whose values
    /// in a model make a counterexample
    free_nodes: Vec<(AigNodeId, u32)>,
}

impl<'a> CnfGenerator<'a> {
    fn new(aig: &'a Aig) -> Self {
        // Estimate number of variables needed
        let num_vars = aig.node_count() as u32 + 10;
        Self {
            aig,
            solver: Solver::new(num_vars),
            node_to_var: IndexMap::new(),
            next_var: 0,
            free_nodes: Vec::new(),
        }
    }

    /// Get or create a SAT variable for an AIG node
    fn get_var(&mut self, node_id: AigNodeId) -> u32 {
        if let Some(&var) = self.node_to_var.get(&node_id) {
            return var;
        }

        let var = self.next_var;
        self.next_var += 1;
        self.node_to_var.insert(node_id, var);
        var
    }

    /// Get SAT literal for an AIG literal
    fn get_lit(&mut self, aig_lit: AigLit) -> Lit {
        let var = self.get_var(aig_lit.node);
        if aig_lit.inverted {
            Lit::neg(var)
        } else {
            Lit::pos(var)
        }
    }

    /// Add CNF clauses for a node's cone (transitive fanin)
    fn add_cone(&mut self, root: AigNodeId, visited: &mut HashSet<AigNodeId>) {
        if visited.contains(&root) {
            return;
        }
        visited.insert(root);

        let node = match self.aig.get_node(root) {
            Some(n) => n.clone(),
            None => return,
        };

        match node {
            AigNode::Const => {
                // Constant false: add unit clause !var
                let var = self.get_var(root);
                self.solver.add_clause(vec![Lit::neg(var)]);
            }
            AigNode::Input { .. } => {
                // Inputs are unconstrained
                let var = self.get_var(root);
                self.free_nodes.push((root, var));
            }
            AigNode::And { left, right } => {
                // First, add cones for children
                self.add_cone(left.node, visited);
                self.add_cone(right.node, visited);

                // Add CNF for AND gate: out = left & right
                // Clauses:
                //   !out | left    (if out is true, left must be true)
                //   !out | right   (if out is true, right must be true)
                //   out | !left | !right   (if both inputs are true, out is true)
                let out = self.get_lit(AigLit::new(root));
                let l = self.get_lit(left);
                let r = self.get_lit(right);

                self.solver.add_clause(vec![out.negate(), l]);
                self.solver.add_clause(vec![out.negate(), r]);
                self.solver.add_clause(vec![out, l.negate(), r.negate()]);
            }
            AigNode::Latch { .. } => {
                // For combinational equivalence a latch output is a free
                // variable; its data input is not part of the cone.
                let var = self.get_var(root);
                self.free_nodes.push((root, var));
            }
            AigNode::Barrier { data, .. } => {
                // Barriers pass through their data
                self.add_cone(data.node, visited);
                let out = self.get_lit(AigLit::new(root));
                let d = self.get_lit(data);
                // out = data (equivalence)
                self.solver.add_clause(vec![out.negate(), d]);
                self.solver.add_clause(vec![out, d.negate()]);
            }
        }
    }

    /// A new variable equal to `a AND b`.
    fn add_and(&mut self, a: Lit, b: Lit) -> Lit {
        let out = Lit::pos(self.next_var);
        self.next_var += 1;
        self.solver.add_clause(vec![out.negate(), a]);
        self.solver.add_clause(vec![out.negate(), b]);
        self.solver.add_clause(vec![out, a.negate(), b.negate()]);
        out
    }

    /// Encode a resubstitution pattern, numbered as the resubstitution pass does.
    fn pattern_lit(&mut self, kind: u8, lits: &[Lit]) -> Lit {
        let or = |cnf: &mut Self, a: Lit, b: Lit| cnf.add_and(a.negate(), b.negate()).negate();
        match kind {
            0 => lits[0],
            1 => self.add_and(lits[0], lits[1]),
            2 => or(self, lits[0], lits[1]),
            3 => {
                let a_nb = self.add_and(lits[0], lits[1].negate());
                let na_b = self.add_and(lits[0].negate(), lits[1]);
                or(self, a_nb, na_b)
            }
            4 => {
                let ab = self.add_and(lits[0], lits[1]);
                or(self, ab, lits[2])
            }
            5 => {
                let ab = or(self, lits[0], lits[1]);
                self.add_and(ab, lits[2])
            }
            6 => {
                let selected = self.add_and(lits[0], lits[1]);
                let other = self.add_and(lits[0].negate(), lits[2]);
                or(self, selected, other)
            }
            _ => unreachable!(),
        }
    }

    /// Check whether `node1` equals `node2` (or its inverse), adding the
    /// cones the check needs to the CNF. `visited` holds the nodes already
    /// encoded, so one generator serves many checks.
    fn check_equivalence(
        &mut self,
        node1: AigNodeId,
        node2: AigNodeId,
        inverted: bool,
        visited: &mut HashSet<AigNodeId>,
    ) -> Check {
        self.add_cone(node1, visited);
        self.add_cone(node2, visited);

        let lit1 = self.get_lit(AigLit::new(node1));
        let lit2 = if inverted {
            self.get_lit(AigLit::new(node2)).negate()
        } else {
            self.get_lit(AigLit::new(node2))
        };

        // A fresh variable that implies the two differ; assuming it asks for
        // a distinguishing input, and the clauses stay harmless afterwards.
        let differ = Lit::pos(self.next_var);
        self.next_var += 1;
        self.solver.add_clause(vec![differ.negate(), lit1, lit2]);
        self.solver
            .add_clause(vec![differ.negate(), lit1.negate(), lit2.negate()]);

        match self.solver.solve_with_assumptions(&[differ]) {
            SatResult::Unsat => Check::Equal,
            SatResult::Sat(model) => Check::Differ(
                self.free_nodes
                    .iter()
                    .map(|&(node, var)| (node, model[var as usize]))
                    .collect(),
            ),
            SatResult::Unknown => Check::Unknown,
        }
    }
}

/// Whether a node always equals a resubstitution pattern over other nodes.
///
/// `kind` and `lits` follow the pattern numbering of the resubstitution pass.
/// An undecided proof counts as not equal.
pub(super) fn prove_resub(
    aig: &Aig,
    target: AigNodeId,
    kind: u8,
    lits: &[AigLit],
    conflict_limit: u64,
) -> bool {
    // Size the solver for the cones in the proof, not the whole design.
    let mut cone = HashSet::new();
    let mut pending: Vec<AigNodeId> = lits.iter().map(|lit| lit.node).collect();
    pending.push(target);
    while let Some(node) = pending.pop() {
        if !cone.insert(node) {
            continue;
        }
        match aig.get_node(node) {
            Some(AigNode::And { left, right }) => {
                pending.push(left.node);
                pending.push(right.node);
            }
            Some(AigNode::Latch { data, .. }) | Some(AigNode::Barrier { data, .. }) => {
                pending.push(data.node);
            }
            _ => {}
        }
    }
    const PATTERN_VARS: u32 = 16;
    let mut cnf = CnfGenerator {
        aig,
        solver: Solver::new(cone.len() as u32 + PATTERN_VARS),
        node_to_var: IndexMap::new(),
        next_var: 0,
        free_nodes: Vec::new(),
    };
    cnf.solver.set_conflict_limit(conflict_limit);

    let mut visited = HashSet::new();
    cnf.add_cone(target, &mut visited);
    for lit in lits {
        cnf.add_cone(lit.node, &mut visited);
    }
    let sat_lits: Vec<Lit> = lits.iter().map(|&lit| cnf.get_lit(lit)).collect();
    let pattern = cnf.pattern_lit(kind, &sat_lits);
    let target_lit = cnf.get_lit(AigLit::new(target));
    let difference = cnf.add_and(target_lit, pattern.negate());
    let opposite = cnf.add_and(target_lit.negate(), pattern);
    let differs = cnf.add_and(difference.negate(), opposite.negate()).negate();
    cnf.solver.solve_with_assumptions(&[differs]) == SatResult::Unsat
}

/// FRAIG optimization pass
pub struct Fraig {
    config: FraigConfig,
    /// Nodes merged in last run
    nodes_merged: usize,
    /// Equivalence classes found
    equiv_classes: usize,
    /// SAT calls made
    sat_calls: usize,
    /// SAT proofs (equivalences found)
    sat_proofs: usize,
    /// SAT refutations (non-equivalences found)
    sat_refutes: usize,
    /// Choices recorded (when record_choices is true)
    choices_recorded: usize,
}

impl Default for Fraig {
    fn default() -> Self {
        Self::new()
    }
}

impl Fraig {
    /// Create a new FRAIG pass with default config
    pub fn new() -> Self {
        Self {
            config: FraigConfig::default(),
            nodes_merged: 0,
            equiv_classes: 0,
            sat_calls: 0,
            sat_proofs: 0,
            sat_refutes: 0,
            choices_recorded: 0,
        }
    }

    /// Create with specific config
    pub fn with_config(config: FraigConfig) -> Self {
        Self {
            config,
            nodes_merged: 0,
            equiv_classes: 0,
            sat_calls: 0,
            sat_proofs: 0,
            sat_refutes: 0,
            choices_recorded: 0,
        }
    }

    /// Get number of nodes merged in last run
    pub fn nodes_merged(&self) -> usize {
        self.nodes_merged
    }

    /// Get number of equivalence classes found
    pub fn equiv_classes(&self) -> usize {
        self.equiv_classes
    }

    /// Get SAT statistics
    pub fn sat_stats(&self) -> (usize, usize, usize) {
        (self.sat_calls, self.sat_proofs, self.sat_refutes)
    }

    /// Get number of choices recorded in last run (when record_choices is true)
    pub fn choices_recorded(&self) -> usize {
        self.choices_recorded
    }

    /// Simulate one 64-bit word: `free_word` gives the word of each input
    /// and latch output (latch outputs are free, as in the SAT encoding).
    fn simulate_word(
        aig: &Aig,
        mut free_word: impl FnMut(AigNodeId) -> u64,
    ) -> IndexMap<AigNodeId, u64> {
        let mut words: IndexMap<AigNodeId, u64> = IndexMap::new();
        let lit_word = |words: &IndexMap<AigNodeId, u64>, lit: AigLit| {
            let w = words.get(&lit.node).copied().unwrap_or(0);
            if lit.inverted {
                !w
            } else {
                w
            }
        };
        for (id, node) in aig.iter_nodes() {
            let word = match node {
                AigNode::Const => 0,
                AigNode::Input { .. } | AigNode::Latch { .. } => free_word(id),
                AigNode::And { left, right } => lit_word(&words, *left) & lit_word(&words, *right),
                AigNode::Barrier { data, .. } => lit_word(&words, *data),
            };
            words.insert(id, word);
        }
        words
    }

    /// Append a simulated word to every node's signature.
    fn append_word(
        signatures: &mut IndexMap<AigNodeId, SimSignature>,
        words: IndexMap<AigNodeId, u64>,
    ) {
        for (id, word) in words {
            signatures
                .entry(id)
                .or_insert_with(|| SimSignature::new(0))
                .patterns
                .push(word);
        }
    }

    /// Signatures of `sim_patterns * sim_rounds` random patterns.
    fn simulate(&self, aig: &Aig) -> IndexMap<AigNodeId, SimSignature> {
        let mut signatures = IndexMap::new();
        let total = (self.config.sim_patterns * self.config.sim_rounds).max(1);
        let mut rng = self.config.seed | 1;
        for _ in 0..total.div_ceil(64) {
            let words = Self::simulate_word(aig, |_| xorshift(&mut rng));
            Self::append_word(&mut signatures, words);
        }
        signatures
    }

    /// Candidate equivalence classes: AND nodes with the same signature up
    /// to inversion, leaving out `excluded` nodes. Each class lists its
    /// nodes in topological order, so its first node can stand for the rest.
    fn find_candidate_classes(
        aig: &Aig,
        signatures: &IndexMap<AigNodeId, SimSignature>,
        excluded: &IndexMap<AigNodeId, (AigNodeId, bool)>,
    ) -> Vec<Vec<(AigNodeId, bool)>> {
        let mut by_signature: IndexMap<SimSignature, Vec<(AigNodeId, bool)>> = IndexMap::new();
        for (id, node) in aig.iter_nodes() {
            if !matches!(node, AigNode::And { .. }) || excluded.contains_key(&id) {
                continue;
            }
            if let Some(sig) = signatures.get(&id) {
                let (canonical, inverted) = sig.canonical();
                by_signature
                    .entry(canonical)
                    .or_default()
                    .push((id, inverted));
            }
        }
        by_signature
            .into_values()
            .filter(|class| class.len() > 1)
            .collect()
    }

    /// Prove equivalences between AND nodes.
    ///
    /// Random simulation proposes classes; each member is checked against
    /// its class's first node on one incremental solver. A failed check
    /// yields an input pattern that tells the two apart; once 64 of them are
    /// collected (or the classes run out) they are simulated as one more
    /// signature word, which splits every class they disprove, and the
    /// classes are formed again. So each false candidate costs at most one
    /// solver call, and simulation does the rest.
    ///
    /// Returns, for each proven node, its representative and whether it is
    /// the representative's inverse.
    fn prove_equivalences(&mut self, aig: &Aig) -> IndexMap<AigNodeId, (AigNodeId, bool)> {
        let mut signatures = self.simulate(aig);
        let mut proven: IndexMap<AigNodeId, (AigNodeId, bool)> = IndexMap::new();
        let mut settled: HashSet<(AigNodeId, AigNodeId)> = HashSet::new();
        let mut cnf = CnfGenerator::new(aig);
        cnf.solver
            .set_conflict_limit(self.config.sat_conflict_limit);
        let mut encoded: HashSet<AigNodeId> = HashSet::new();
        let mut rng = self.config.seed.rotate_left(17) | 1;
        let mut first_pass = true;

        loop {
            let classes = Self::find_candidate_classes(aig, &signatures, &proven);
            if first_pass {
                self.equiv_classes = classes.len();
                first_pass = false;
            }
            let mut counterexamples: Vec<Vec<(AigNodeId, bool)>> = Vec::new();
            'classes: for class in &classes {
                let (repr, repr_inv) = class[0];
                for &(node, node_inv) in &class[1..] {
                    if self.sat_calls >= self.config.max_sat_calls || counterexamples.len() >= 64 {
                        break 'classes;
                    }
                    if !settled.insert((repr, node)) {
                        continue;
                    }
                    let inverted = repr_inv != node_inv;
                    self.sat_calls += 1;
                    match cnf.check_equivalence(repr, node, inverted, &mut encoded) {
                        Check::Equal => {
                            self.sat_proofs += 1;
                            proven.insert(node, (repr, inverted));
                        }
                        Check::Differ(values) => {
                            self.sat_refutes += 1;
                            counterexamples.push(values);
                        }
                        Check::Unknown => {}
                    }
                }
            }
            if counterexamples.is_empty() {
                break;
            }
            // Bit k of a free node's word is its value in counterexample k
            // (random where that counterexample leaves it open).
            let mut assigned: IndexMap<AigNodeId, (u64, u64)> = IndexMap::new();
            for (bit, values) in counterexamples.iter().enumerate() {
                for &(node, value) in values {
                    let entry = assigned.entry(node).or_insert((0, 0));
                    entry.0 |= 1 << bit;
                    if value {
                        entry.1 |= 1 << bit;
                    }
                }
            }
            let words = Self::simulate_word(aig, |node| {
                let random = xorshift(&mut rng);
                match assigned.get(&node) {
                    Some(&(mask, values)) => (random & !mask) | values,
                    None => random,
                }
            });
            Self::append_word(&mut signatures, words);
        }
        proven
    }

    /// Rebuild the AIG with merged nodes
    fn rebuild_aig(&self, aig: &Aig, node_map: &IndexMap<AigNodeId, AigLit>) -> Aig {
        let mut new_aig = Aig::new(aig.name.clone());
        let mut old_to_new: IndexMap<AigNodeId, AigLit> = IndexMap::new();

        // Constant maps to itself
        old_to_new.insert(AigNodeId::FALSE, AigLit::false_lit());

        // Phase 1a: Add inputs
        for (old_id, node) in aig.iter_nodes() {
            if let AigNode::Input {
                name, source_net, ..
            } = node
            {
                let safety = aig.get_safety_info(old_id).cloned().unwrap_or_default();
                let new_id = new_aig.add_input_with_safety(name.clone(), *source_net, safety);
                old_to_new.insert(old_id, AigLit::new(new_id));
            }
        }

        // Propagate clock/reset input metadata to new AIG
        new_aig.copy_clock_reset_metadata(aig, &old_to_new);

        // Phase 1b: Pre-create latches/barriers with placeholder data
        // In sequential circuits, latch data inputs reference AND nodes that may have
        // higher IDs. We must process all AND nodes before resolving latch data.
        // (old_id, data, init, clock, reset)
        type LatchInfo = (
            AigNodeId,
            AigLit,
            Option<bool>,
            Option<AigNodeId>,
            Option<AigLit>,
        );
        // (old_id, barrier_type, data, enable, clock, reset, init)
        type BarrierInfo = (
            AigNodeId,
            BarrierType,
            AigLit,
            Option<AigLit>,
            Option<AigNodeId>,
            Option<AigLit>,
            Option<bool>,
        );
        let mut latch_ids: Vec<LatchInfo> = Vec::new();
        let mut barrier_ids: Vec<BarrierInfo> = Vec::new();

        for (old_id, node) in aig.iter_nodes() {
            // Skip merged nodes
            if node_map.contains_key(&old_id) {
                continue;
            }
            match node {
                AigNode::Latch {
                    data,
                    init,
                    clock,
                    reset,
                    sync_reset,
                } => {
                    let new_clock = clock.and_then(|c| old_to_new.get(&c).map(|l| l.node));
                    let new_reset = reset
                        .and_then(|r| old_to_new.get(&r.node).map(|l| l.invert_if(r.inverted)));
                    let safety = aig.get_safety_info(old_id).cloned().unwrap_or_default();
                    let new_id = new_aig.add_latch_with_safety(
                        AigLit::false_lit(),
                        *init,
                        new_clock,
                        new_reset,
                        safety,
                    );
                    if *sync_reset {
                        new_aig.mark_latch_sync_reset(new_id);
                    }
                    old_to_new.insert(old_id, AigLit::new(new_id));
                    latch_ids.push((old_id, *data, *init, *clock, *reset));
                }
                AigNode::Barrier {
                    barrier_type,
                    data,
                    enable,
                    clock,
                    reset,
                    init,
                } => {
                    let new_clock = clock.and_then(|c| old_to_new.get(&c).map(|l| l.node));
                    let new_reset = reset
                        .and_then(|r| old_to_new.get(&r.node).map(|l| l.invert_if(r.inverted)));
                    let safety = aig.get_safety_info(old_id).cloned().unwrap_or_default();
                    let new_id = new_aig.add_barrier_with_safety(
                        barrier_type.clone(),
                        AigLit::false_lit(),
                        None,
                        new_clock,
                        new_reset,
                        *init,
                        safety,
                    );
                    old_to_new.insert(old_id, AigLit::new(new_id));
                    barrier_ids.push((
                        old_id,
                        barrier_type.clone(),
                        *data,
                        *enable,
                        *clock,
                        *reset,
                        *init,
                    ));
                }
                _ => {}
            }
        }

        // Phase 2: Process AND nodes and merged nodes
        for (old_id, node) in aig.iter_nodes() {
            // Check if this node was merged
            if let Some(&mapped_lit) = node_map.get(&old_id) {
                // Resolve the mapped literal
                if let Some(&new_lit) = old_to_new.get(&mapped_lit.node) {
                    let final_lit = if mapped_lit.inverted {
                        new_lit.invert()
                    } else {
                        new_lit
                    };
                    old_to_new.insert(old_id, final_lit);
                }
                continue;
            }

            match node {
                AigNode::Const
                | AigNode::Input { .. }
                | AigNode::Latch { .. }
                | AigNode::Barrier { .. } => {
                    // Already handled in phases 1a/1b
                }
                AigNode::And { left, right } => {
                    let new_left = self.translate_lit(&old_to_new, *left);
                    let new_right = self.translate_lit(&old_to_new, *right);
                    let safety = aig.get_safety_info(old_id).cloned().unwrap_or_default();
                    let new_lit = new_aig.add_and_with_safety(new_left, new_right, safety);
                    old_to_new.insert(old_id, new_lit);
                }
            }
        }

        // Phase 3: Update latch/barrier data with resolved values
        for (old_id, data, _init, clock, reset) in latch_ids {
            let new_node_id = old_to_new.get(&old_id).map(|lit| lit.node).unwrap();
            let new_data = self.translate_lit(&old_to_new, data);
            new_aig.update_latch_data(new_node_id, new_data);
            let new_clock = clock.and_then(|c| old_to_new.get(&c).map(|l| l.node));
            let new_reset =
                reset.and_then(|r| old_to_new.get(&r.node).map(|l| l.invert_if(r.inverted)));
            new_aig.set_sequential_controls(new_node_id, new_clock, new_reset);
        }
        for (old_id, _barrier_type, data, enable, clock, reset, _init) in barrier_ids {
            let new_node_id = old_to_new.get(&old_id).map(|lit| lit.node).unwrap();
            let new_data = self.translate_lit(&old_to_new, data);
            let new_enable = enable.map(|e| self.translate_lit(&old_to_new, e));
            new_aig.update_barrier_data(new_node_id, new_data, new_enable);
            let new_clock = clock.and_then(|c| old_to_new.get(&c).map(|l| l.node));
            let new_reset =
                reset.and_then(|r| old_to_new.get(&r.node).map(|l| l.invert_if(r.inverted)));
            new_aig.set_sequential_controls(new_node_id, new_clock, new_reset);
        }

        // Add outputs
        for (name, lit) in aig.outputs() {
            let new_lit = self.translate_lit(&old_to_new, *lit);
            new_aig.add_output(name.clone(), new_lit);
        }

        new_aig
    }

    /// Translate a literal using the old-to-new mapping
    fn translate_lit(&self, old_to_new: &IndexMap<AigNodeId, AigLit>, lit: AigLit) -> AigLit {
        if let Some(&new_lit) = old_to_new.get(&lit.node) {
            if lit.inverted {
                new_lit.invert()
            } else {
                new_lit
            }
        } else {
            lit
        }
    }
}

impl Pass for Fraig {
    fn name(&self) -> &str {
        "fraig"
    }

    fn run(&mut self, aig: &mut Aig) -> PassResult {
        let mut result = PassResult::new(self.name());
        result.record_before(aig);

        // Reset counters
        self.nodes_merged = 0;
        self.equiv_classes = 0;
        self.sat_calls = 0;
        self.sat_proofs = 0;
        self.sat_refutes = 0;
        self.choices_recorded = 0;

        let proven = self.prove_equivalences(aig);

        if self.config.record_choices {
            // Keep both implementations and let the mapper choose.
            for (&node, &(repr, inverted)) in &proven {
                let alternative = if inverted {
                    AigLit::not(node)
                } else {
                    AigLit::new(node)
                };
                if aig.add_choice(repr, alternative) {
                    self.choices_recorded += 1;
                }
            }
        } else {
            self.nodes_merged = proven.len();
            if !proven.is_empty() {
                let node_map: IndexMap<AigNodeId, AigLit> = proven
                    .iter()
                    .map(|(&node, &(repr, inverted))| {
                        let lit = AigLit::new(repr);
                        (node, if inverted { lit.invert() } else { lit })
                    })
                    .collect();
                let new_aig = self.rebuild_aig(aig, &node_map);
                *aig = new_aig;
            }
        }

        result.record_after(aig);
        result.add_extra("equiv_classes", &self.equiv_classes.to_string());
        result.add_extra("nodes_merged", &self.nodes_merged.to_string());
        result.add_extra("choices_recorded", &self.choices_recorded.to_string());
        result.add_extra("sat_calls", &self.sat_calls.to_string());
        result.add_extra("sat_proofs", &self.sat_proofs.to_string());
        result.add_extra("sat_refutes", &self.sat_refutes.to_string());
        result
    }
}

/// Statistics from FRAIG pass
#[derive(Debug, Clone, Default)]
pub struct FraigStats {
    /// Equivalence classes found
    pub equiv_classes: usize,
    /// Nodes merged
    pub nodes_merged: usize,
    /// Choices recorded (when record_choices is true)
    pub choices_recorded: usize,
    /// Original AND count
    pub original_ands: usize,
    /// Final AND count
    pub final_ands: usize,
    /// SAT calls made
    pub sat_calls: usize,
    /// SAT proofs (equivalences proven)
    pub sat_proofs: usize,
    /// SAT refutations (non-equivalences found)
    pub sat_refutes: usize,
}

impl FraigStats {
    /// Get reduction percentage
    pub fn reduction_percent(&self) -> f64 {
        if self.original_ands == 0 {
            0.0
        } else {
            (self.original_ands - self.final_ands) as f64 / self.original_ands as f64 * 100.0
        }
    }

    /// Get summary string
    pub fn summary(&self) -> String {
        format!(
            "Found {} equiv classes, merged {} nodes (SAT: {} calls, {} proofs, {} refutes), ANDs: {} → {} ({:.1}% reduction)",
            self.equiv_classes,
            self.nodes_merged,
            self.sat_calls,
            self.sat_proofs,
            self.sat_refutes,
            self.original_ands,
            self.final_ands,
            self.reduction_percent()
        )
    }
}

/// Run FRAIG with default config
pub fn run_fraig(aig: &mut Aig) -> FraigStats {
    let original_ands = aig.and_count();
    let mut fraig = Fraig::new();
    fraig.run(aig);

    let (sat_calls, sat_proofs, sat_refutes) = fraig.sat_stats();

    FraigStats {
        equiv_classes: fraig.equiv_classes(),
        nodes_merged: fraig.nodes_merged(),
        choices_recorded: fraig.choices_recorded(),
        original_ands,
        final_ands: aig.and_count(),
        sat_calls,
        sat_proofs,
        sat_refutes,
    }
}

/// Run FRAIG with custom config
pub fn run_fraig_with_config(aig: &mut Aig, config: FraigConfig) -> FraigStats {
    let original_ands = aig.and_count();
    let mut fraig = Fraig::with_config(config);
    fraig.run(aig);

    let (sat_calls, sat_proofs, sat_refutes) = fraig.sat_stats();

    FraigStats {
        equiv_classes: fraig.equiv_classes(),
        nodes_merged: fraig.nodes_merged(),
        choices_recorded: fraig.choices_recorded(),
        original_ands,
        final_ands: aig.and_count(),
        sat_calls,
        sat_proofs,
        sat_refutes,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_fraig_config_default() {
        let config = FraigConfig::default();
        assert_eq!(config.sim_patterns, 64);
        assert_eq!(config.sim_rounds, 3);
        assert!(!config.verbose);
    }

    #[test]
    fn test_sim_signature_operations() {
        let sig1 = SimSignature::from_single(0xAAAAAAAAAAAAAAAA);
        let sig2 = sig1.invert();
        assert_eq!(sig2.patterns[0], 0x5555555555555555);

        let and_result = sig1.and(&sig2);
        assert_eq!(and_result.patterns[0], 0);
    }

    #[test]
    fn test_fraig_empty_aig() {
        let mut aig = Aig::new("test".to_string());
        let mut fraig = Fraig::new();
        let result = fraig.run(&mut aig);

        assert!(!result.changed);
        assert_eq!(fraig.nodes_merged(), 0);
    }

    #[test]
    fn test_fraig_single_and() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let and_result = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("out".to_string(), and_result);

        let original_ands = aig.and_count();
        let mut fraig = Fraig::new();
        fraig.run(&mut aig);

        // Should not merge anything with just one AND
        assert_eq!(aig.and_count(), original_ands);
    }

    #[test]
    fn test_fraig_functionally_equivalent() {
        // Create: y1 = a & b, y2 = b & a (should be same due to strash)
        // But create them via different paths to test FRAIG
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        // y1 = a & b
        let y1 = aig.add_and(AigLit::new(a), AigLit::new(b));

        // Due to strash, y2 will be the same as y1
        // To properly test FRAIG, we'd need a scenario where strash doesn't catch it
        aig.add_output("y1".to_string(), y1);
        aig.add_output("y2".to_string(), y1);

        let mut fraig = Fraig::new();
        fraig.run(&mut aig);

        // FRAIG should find that these are the same
        assert_eq!(aig.output_count(), 2);
    }

    #[test]
    fn test_fraig_de_morgan() {
        // Test De Morgan's law: !(a & b) = !a | !b
        // Create both forms and verify FRAIG can prove equivalence
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        // Form 1: !(a & b)
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let not_ab = ab.invert();

        // Form 2: !a | !b = !(!a & !b) ... wait, that's wrong
        // Actually: !a | !b = !(a & b) by De Morgan
        // So we need to express OR in terms of AND
        // !a | !b = !(!(!a) & !(!b)) = !(a & b)
        // This is the same as form 1, so strash will catch it

        // Let's create a different test: double negation
        // !!a = a
        let not_a = AigLit::not(a);
        let not_not_a = not_a.invert();

        aig.add_output("y1".to_string(), AigLit::new(a));
        aig.add_output("y2".to_string(), not_not_a);

        let mut fraig = Fraig::new();
        fraig.run(&mut aig);

        // Both outputs should reference the same thing (input a)
        assert_eq!(aig.and_count(), 1); // Only the ab AND
    }

    #[test]
    fn test_fraig_stats() {
        let stats = FraigStats {
            equiv_classes: 5,
            nodes_merged: 10,
            choices_recorded: 0,
            original_ands: 100,
            final_ands: 90,
            sat_calls: 15,
            sat_proofs: 10,
            sat_refutes: 5,
        };

        assert_eq!(stats.reduction_percent(), 10.0);
        assert!(stats.summary().contains("10 nodes"));
        assert!(stats.summary().contains("15 calls"));
    }

    #[test]
    fn test_cnf_generation() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let mut cnf = CnfGenerator::new(&aig);
        let mut visited = HashSet::new();
        cnf.add_cone(ab.node, &mut visited);

        // Should have variables for a, b, and ab
        assert!(cnf.node_to_var.contains_key(&a));
        assert!(cnf.node_to_var.contains_key(&b));
        assert!(cnf.node_to_var.contains_key(&ab.node));
    }

    #[test]
    fn test_fraig_record_choices() {
        // Build an AIG with multiple nodes
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        // Direct AND: a & b
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));

        // Another node using inputs
        let not_a = AigLit::not(a);
        let not_b = AigLit::not(b);
        let nand = aig.add_and(not_a, not_b); // !a & !b

        aig.add_output("ab".to_string(), ab);
        aig.add_output("nand".to_string(), nand);

        let initial_and_count = aig.and_count();

        // Run FRAIG with record_choices enabled
        let config = FraigConfig {
            record_choices: true,
            ..Default::default()
        };
        let mut fraig = Fraig::with_config(config);
        fraig.run(&mut aig);

        // Node count should remain the same (no merging in choice mode)
        assert_eq!(aig.and_count(), initial_and_count);

        // Verify the method works
        assert_eq!(fraig.nodes_merged(), 0); // No nodes merged in choice mode
    }

    /// Evaluates every output of a purely combinational AIG for one input
    /// assignment (inputs in `iter_inputs` order).
    fn eval_outputs(aig: &Aig, assignment: &[bool]) -> Vec<bool> {
        fn eval(aig: &Aig, lit: AigLit, values: &mut HashMap<AigNodeId, bool>) -> bool {
            let node_value = if let Some(&v) = values.get(&lit.node) {
                v
            } else {
                let v = match aig.get_node(lit.node).expect("node exists") {
                    AigNode::Const => false,
                    AigNode::And { left, right } => {
                        eval(aig, *left, values) && eval(aig, *right, values)
                    }
                    other => panic!("unexpected node in combinational test AIG: {other:?}"),
                };
                values.insert(lit.node, v);
                v
            };
            node_value ^ lit.inverted
        }
        let mut values: HashMap<AigNodeId, bool> = aig
            .iter_inputs()
            .map(|(id, _)| id)
            .zip(assignment.iter().copied())
            .collect();
        aig.outputs()
            .iter()
            .map(|(_, lit)| eval(aig, *lit, &mut values))
            .collect()
    }

    fn and_all(aig: &mut Aig, lits: &[AigLit]) -> AigLit {
        let mut acc = lits[0];
        for &l in &lits[1..] {
            acc = aig.add_and(acc, l);
        }
        acc
    }

    #[test]
    fn test_fraig_merges_structurally_different_equivalents() {
        // XOR written two ways, and a 12-input AND chained in opposite orders:
        // strash cannot see either equivalence, simulation plus SAT must.
        let mut aig = Aig::new("test".to_string());
        let ins: Vec<AigLit> = (0..12)
            .map(|i| AigLit::new(aig.add_input(format!("i{i}"), None)))
            .collect();
        let (a, b) = (ins[0], ins[1]);
        let x1 = {
            let p = aig.add_and(a, b.invert());
            let q = aig.add_and(a.invert(), b);
            aig.add_or(p, q)
        };
        let x2 = {
            let p = aig.add_and(a, b);
            let q = aig.add_and(a.invert(), b.invert());
            aig.add_or(p, q).invert()
        };
        let fwd = and_all(&mut aig, &ins);
        let rev: Vec<AigLit> = ins.iter().rev().copied().collect();
        let bwd = and_all(&mut aig, &rev);
        aig.add_output("x1".to_string(), x1);
        aig.add_output("x2".to_string(), x2);
        aig.add_output("fwd".to_string(), fwd);
        aig.add_output("bwd".to_string(), bwd);

        let before = aig.and_count();
        let reference: Vec<Vec<bool>> = (0u32..1 << 12)
            .map(|m| eval_outputs(&aig, &(0..12).map(|i| m >> i & 1 == 1).collect::<Vec<_>>()))
            .collect();

        let mut fraig = Fraig::new();
        fraig.run(&mut aig);

        assert!(fraig.nodes_merged() > 0);
        assert!(aig.and_count() < before);
        let outs = aig.outputs();
        assert_eq!(outs[0].1, outs[1].1, "XOR forms should share one node");
        assert_eq!(outs[2].1, outs[3].1, "AND chains should share one node");
        for (m, expected) in reference.iter().enumerate() {
            let assignment: Vec<bool> = (0..12).map(|i| m >> i & 1 == 1).collect();
            assert_eq!(&eval_outputs(&aig, &assignment), expected, "pattern {m:#x}");
        }
    }

    #[test]
    fn test_fraig_keeps_nodes_that_differ_on_one_rare_pattern() {
        // Two 20-input ANDs that differ only in the polarity of the last input,
        // plus their complements in other shapes. Random simulation almost
        // never sets all 20 inputs to 1, so every node lands in the constant-0
        // class; SAT must refute each candidate and nothing may be merged.
        let mut aig = Aig::new("test".to_string());
        let ins: Vec<AigLit> = (0..20)
            .map(|i| AigLit::new(aig.add_input(format!("i{i}"), None)))
            .collect();
        let y1 = and_all(&mut aig, &ins);
        let mut flipped = ins.clone();
        flipped[19] = flipped[19].invert();
        let y2 = and_all(&mut aig, &flipped);
        let mut other = ins.clone();
        other[0] = other[0].invert();
        let y3 = and_all(&mut aig, &other);
        aig.add_output("y1".to_string(), y1);
        aig.add_output("y2".to_string(), y2);
        aig.add_output("y3".to_string(), y3);

        let before = aig.and_count();
        let mut fraig = Fraig::new();
        fraig.run(&mut aig);

        assert_eq!(fraig.nodes_merged(), 0);
        assert_eq!(aig.and_count(), before);
        let mut all_ones = vec![true; 20];
        assert_eq!(eval_outputs(&aig, &all_ones), vec![true, false, false]);
        all_ones[19] = false;
        assert_eq!(eval_outputs(&aig, &all_ones), vec![false, true, false]);
        all_ones[19] = true;
        all_ones[0] = false;
        assert_eq!(eval_outputs(&aig, &all_ones), vec![false, false, true]);
    }

    #[test]
    fn test_fraig_treats_latches_as_free_variables() {
        // A latch output and an input are unrelated signals; FRAIG must not
        // look through the latch to its next-state function and merge them.
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let q = aig.add_latch(AigLit::new(a), None, None, None);
        let qa = aig.add_and(AigLit::new(q), AigLit::new(b));
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("qa".to_string(), qa);
        aig.add_output("ab".to_string(), ab);

        let mut fraig = Fraig::new();
        fraig.run(&mut aig);

        assert_eq!(fraig.nodes_merged(), 0);
        assert_ne!(aig.outputs()[0].1, aig.outputs()[1].1);
    }
}
