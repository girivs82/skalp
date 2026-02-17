//! Algebraic/arithmetic solver for multiplier equivalence verification.
//!
//! SAT solving on multiplier XOR miters is co-NP hard — even with unlimited conflict
//! budgets, SAT solvers take exponential time on multiplier cones. This module implements
//! backward polynomial rewriting over Z with the boolean constraint x²=x, which verifies
//! multipliers in polynomial time.
//!
//! Algorithm:
//! 1. Group unresolved diff gates by base signal name (e.g., output_reg[0..31])
//! 2. Form word-level polynomial: W = Σ 2^i * (var(mir_node_i) - var(gate_node_i))
//! 3. Backward rewrite: process AND nodes in reverse topological order,
//!    substituting each node variable with its gate equation polynomial (a·b)
//! 4. If W reduces to zero, the group is proven equivalent

use std::collections::{BTreeMap, BTreeSet, HashSet};
use crate::equivalence::{Aig, AigLit, AigNode};
use rayon::prelude::*;

/// Maximum number of terms before bailing out (prevents exponential blowup on non-multiplier cones)
const TERM_LIMIT: usize = 1_000_000;

/// Maximum number of working terms — if W exceeds this after any substitution, bail out.
/// Pure multiplier rewrites stay O(n²) ≈ 1-10K terms for 32-bit. If we're above this,
/// the cone isn't a clean multiplier and the algorithm won't converge.
const WORKING_TERM_LIMIT: usize = 100_000;

/// Maximum cone size (AND nodes) before skipping algebraic verification for a group
const MAX_CONE_SIZE: usize = 200_000;

/// Per-group time limit in seconds
const GROUP_TIMEOUT_SECS: u64 = 30;

// ── Public result types ─────────────────────────────────────────────────────

/// Status of algebraic verification for a single group of related diff gates
#[derive(Debug, Clone, serde::Serialize)]
pub enum AlgebraicGroupStatus {
    /// Group proven equivalent (W reduced to zero)
    Proven { substitutions: usize, time_ms: u64 },
    /// Could not prove — includes diagnostic reason
    Unknown { reason: String, cone_size: usize, terms_at_failure: usize },
}

/// Report for a single group of diff gates
#[derive(Debug, Clone, serde::Serialize)]
pub struct AlgebraicGroupReport {
    pub name: String,
    pub num_bits: usize,
    pub status: AlgebraicGroupStatus,
}

/// Complete result of algebraic verification across all groups
#[derive(Debug, Clone, serde::Serialize)]
pub struct AlgebraicVerificationResult {
    pub proven_names: Vec<String>,
    pub group_results: Vec<AlgebraicGroupReport>,
    pub total_time_ms: u64,
}

// ── Polynomial implementation ───────────────────────────────────────────────

/// A polynomial over Z with boolean variables (x²=x constraint).
/// Each term is a monomial (sorted set of variable IDs) with an integer coefficient.
/// Tracks active variable IDs for fast "contains variable" queries.
#[derive(Clone, Debug)]
struct Polynomial {
    /// Map from monomial (sorted variable IDs) → coefficient
    terms: BTreeMap<Vec<u32>, i64>,
    /// Set of all variable IDs currently appearing in any term (for fast lookups)
    active_vars: HashSet<u32>,
}

impl Polynomial {
    /// The zero polynomial
    fn zero() -> Self {
        Polynomial {
            terms: BTreeMap::new(),
            active_vars: HashSet::new(),
        }
    }

    /// The constant polynomial c
    fn constant(c: i64) -> Self {
        if c == 0 {
            return Self::zero();
        }
        let mut terms = BTreeMap::new();
        terms.insert(Vec::new(), c);
        Polynomial {
            terms,
            active_vars: HashSet::new(),
        }
    }

    /// A single variable: 1·x
    fn var(id: u32) -> Self {
        let mut terms = BTreeMap::new();
        terms.insert(vec![id], 1);
        let mut active_vars = HashSet::new();
        active_vars.insert(id);
        Polynomial { terms, active_vars }
    }

    /// Check if polynomial is zero
    fn is_zero(&self) -> bool {
        self.terms.is_empty()
    }

    /// Number of terms
    fn term_count(&self) -> usize {
        self.terms.len()
    }

    /// Check if a variable appears in any term (O(1) via active_vars set)
    fn contains_var(&self, var_id: u32) -> bool {
        self.active_vars.contains(&var_id)
    }

    /// Rebuild active_vars from terms (used after bulk operations)
    fn rebuild_active_vars(&mut self) {
        self.active_vars.clear();
        for mono in self.terms.keys() {
            for &v in mono {
                self.active_vars.insert(v);
            }
        }
    }

    /// Add two polynomials: self + other
    fn add(&self, other: &Polynomial) -> Polynomial {
        let mut result = self.terms.clone();
        for (mono, &coeff) in &other.terms {
            let entry = result.entry(mono.clone()).or_insert(0);
            *entry += coeff;
            if *entry == 0 {
                result.remove(mono);
            }
        }
        let mut p = Polynomial {
            terms: result,
            active_vars: HashSet::new(),
        };
        p.rebuild_active_vars();
        p
    }

    /// Subtract: self - other
    fn sub(&self, other: &Polynomial) -> Polynomial {
        let mut result = self.terms.clone();
        for (mono, &coeff) in &other.terms {
            let entry = result.entry(mono.clone()).or_insert(0);
            *entry -= coeff;
            if *entry == 0 {
                result.remove(mono);
            }
        }
        let mut p = Polynomial {
            terms: result,
            active_vars: HashSet::new(),
        };
        p.rebuild_active_vars();
        p
    }

    /// Scale by integer constant
    fn scale(&self, c: i64) -> Polynomial {
        if c == 0 {
            return Self::zero();
        }
        let terms: BTreeMap<Vec<u32>, i64> = self
            .terms
            .iter()
            .map(|(mono, &coeff)| (mono.clone(), coeff * c))
            .filter(|(_, c)| *c != 0)
            .collect();
        Polynomial {
            terms,
            active_vars: self.active_vars.clone(),
        }
    }

    /// Multiply two polynomials with boolean constraint x²=x.
    /// Returns None if term limit is exceeded.
    fn mul(&self, other: &Polynomial) -> Option<Polynomial> {
        self.mul_with_limit(other, TERM_LIMIT)
    }

    /// Multiply two polynomials with boolean constraint x²=x.
    /// Returns None if `limit` terms is exceeded (allows early bail).
    fn mul_with_limit(&self, other: &Polynomial, limit: usize) -> Option<Polynomial> {
        if self.is_zero() || other.is_zero() {
            return Some(Self::zero());
        }

        let mut result: BTreeMap<Vec<u32>, i64> = BTreeMap::new();

        for (mono_a, &coeff_a) in &self.terms {
            for (mono_b, &coeff_b) in &other.terms {
                // Monomial product: union of variable sets (x²=x means no duplicates matter)
                let merged = merge_monomials(mono_a, mono_b);
                let coeff = coeff_a * coeff_b;
                let entry = result.entry(merged).or_insert(0);
                *entry += coeff;
                if *entry == 0 {
                    // Need to re-compute the key to remove it
                    let key = merge_monomials(mono_a, mono_b);
                    result.remove(&key);
                }

                if result.len() > limit {
                    return None; // Bail out
                }
            }
        }

        // Clean up zero coefficients
        result.retain(|_, c| *c != 0);
        let mut p = Polynomial {
            terms: result,
            active_vars: HashSet::new(),
        };
        p.rebuild_active_vars();
        Some(p)
    }

    /// Substitute a variable: replace all occurrences of `var_id` with `replacement` polynomial.
    /// Uses the identity: mono containing var = (mono without var) * replacement
    /// Returns None if term limit is exceeded or deadline is reached.
    fn substitute(&self, var_id: u32, replacement: &Polynomial, deadline: std::time::Instant) -> Option<Polynomial> {
        if !self.contains_var(var_id) {
            return Some(self.clone());
        }

        // Accumulate directly into a BTreeMap to avoid O(n²) from repeated rebuild_active_vars
        let mut result_terms: BTreeMap<Vec<u32>, i64> = BTreeMap::new();
        let mut iter_count = 0usize;

        for (mono, &coeff) in &self.terms {
            // Check deadline every 500 iterations within substitute
            iter_count += 1;
            if iter_count % 500 == 0 && std::time::Instant::now() >= deadline {
                return None;
            }

            if mono.contains(&var_id) {
                // This term contains the variable — split it out and multiply by replacement
                let remaining: Vec<u32> = mono.iter().filter(|&&v| v != var_id).cloned().collect();
                // Expand: coeff * (remaining monomial) * replacement
                for (rep_mono, &rep_coeff) in &replacement.terms {
                    let merged = merge_monomials(&remaining, rep_mono);
                    let product_coeff = coeff * rep_coeff;
                    let entry = result_terms.entry(merged).or_insert(0);
                    *entry += product_coeff;
                    if *entry == 0 {
                        // Need to remove — but entry API consumed it; just leave for cleanup
                    }
                }
            } else {
                // This term doesn't contain the variable — keep as-is
                let entry = result_terms.entry(mono.clone()).or_insert(0);
                *entry += coeff;
            }

            if result_terms.len() > TERM_LIMIT {
                return None;
            }
        }

        // Clean up zeros and rebuild active_vars once
        result_terms.retain(|_, c| *c != 0);
        let mut result = Polynomial {
            terms: result_terms,
            active_vars: HashSet::new(),
        };
        result.rebuild_active_vars();
        Some(result)
    }
}

/// Merge two sorted monomial variable lists (set union, maintaining sorted order).
/// Since x²=x, duplicate variables are kept only once.
fn merge_monomials(a: &[u32], b: &[u32]) -> Vec<u32> {
    let mut result = Vec::with_capacity(a.len() + b.len());
    let mut i = 0;
    let mut j = 0;
    while i < a.len() && j < b.len() {
        match a[i].cmp(&b[j]) {
            std::cmp::Ordering::Less => {
                result.push(a[i]);
                i += 1;
            }
            std::cmp::Ordering::Greater => {
                result.push(b[j]);
                j += 1;
            }
            std::cmp::Ordering::Equal => {
                result.push(a[i]); // x²=x: keep just one copy
                i += 1;
                j += 1;
            }
        }
    }
    result.extend_from_slice(&a[i..]);
    result.extend_from_slice(&b[j..]);
    result
}

/// Convert an AIG literal to a polynomial.
/// - Non-inverted: var(node_id)
/// - Inverted: 1 - var(node_id)
/// - Constant false (node 0): 0
/// - Constant true (node 0 inverted): 1
fn lit_to_poly(lit: &AigLit) -> Polynomial {
    if lit.node.0 == 0 {
        // Constant false node
        if lit.inverted {
            Polynomial::constant(1) // ~false = true
        } else {
            Polynomial::zero() // false = 0
        }
    } else if lit.inverted {
        // 1 - var
        Polynomial::constant(1).sub(&Polynomial::var(lit.node.0))
    } else {
        Polynomial::var(lit.node.0)
    }
}

/// Group unresolved diff gates by base signal name.
/// E.g., "diff_output[top.voltage_loop.output_reg[5]]" → base="top.voltage_loop.output_reg", bit=5
/// Returns: BTreeMap<base_name, Vec<(miter_output_index, full_name, bit_index)>>
fn group_gates_by_signal(unresolved: &[(usize, String)]) -> BTreeMap<String, Vec<(usize, String, u32)>> {
    let mut groups: BTreeMap<String, Vec<(usize, String, u32)>> = BTreeMap::new();

    for (idx, name) in unresolved {
        // Extract the signal name from "diff_output[...]" or "diff_next_state[...]"
        let inner = if let Some(s) = name.strip_prefix("diff_output[") {
            s.strip_suffix(']')
        } else if let Some(s) = name.strip_prefix("diff_next_state[") {
            s.strip_suffix(']')
        } else {
            None
        };

        let inner = match inner {
            Some(s) => s,
            None => {
                // Can't parse — put in its own group
                groups.entry(name.clone()).or_default().push((*idx, name.clone(), 0));
                continue;
            }
        };

        // Check if it ends with [N] for a bit index
        if let Some(bracket_pos) = inner.rfind('[') {
            let base = &inner[..bracket_pos];
            let bit_str = &inner[bracket_pos + 1..];
            if let Some(bit_str) = bit_str.strip_suffix(']') {
                if let Ok(bit) = bit_str.parse::<u32>() {
                    groups.entry(base.to_string()).or_default().push((*idx, name.clone(), bit));
                    continue;
                }
            }
        }

        // No bit index — single-bit signal, bit_index=0
        groups.entry(inner.to_string()).or_default().push((*idx, name.clone(), 0));
    }

    groups
}

/// Collect all AND nodes in the backward cone of the given node IDs.
/// Returns node IDs in reverse topological order (highest first, since AIG is topo-sorted).
/// Returns None if cone exceeds MAX_CONE_SIZE.
fn collect_cone_nodes(miter: &Aig, root_node_ids: &BTreeSet<u32>) -> Option<Vec<u32>> {
    let mut in_cone: HashSet<u32> = HashSet::new();
    let mut and_count = 0usize;

    // DFS backward from roots
    let mut stack: Vec<u32> = root_node_ids.iter().cloned().collect();
    while let Some(node_id) = stack.pop() {
        if node_id == 0 || !in_cone.insert(node_id) {
            continue;
        }
        if let Some(AigNode::And { left, right }) = miter.nodes.get(node_id as usize) {
            and_count += 1;
            if and_count > MAX_CONE_SIZE {
                return None; // Cone too large
            }
            if left.node.0 != 0 {
                stack.push(left.node.0);
            }
            if right.node.0 != 0 {
                stack.push(right.node.0);
            }
        }
    }

    // Return in reverse topological order (highest node index first)
    let mut nodes: Vec<u32> = in_cone.into_iter().collect();
    nodes.sort_unstable_by(|a, b| b.cmp(a));
    Some(nodes)
}

/// Result of algebraic verification for a single group (internal)
enum AlgebraicResult {
    Proven { substitutions: usize, time_ms: u64 },
    Unknown { reason: String, cone_size: usize, terms_at_failure: usize },
}

/// Verify a word-level group of related diff gates using backward polynomial rewriting.
///
/// Forms W = Σ 2^bit * (mir_poly - gate_poly) and substitutes AND gates backward.
/// If W reduces to 0, all bits in the group are proven equivalent.
fn verify_word_level_group(
    miter: &Aig,
    group: &[(usize, String, u32)],  // (miter_output_idx, name, bit_index)
    matched_pairs: &[(String, AigLit, AigLit)],
) -> AlgebraicResult {
    let group_start = std::time::Instant::now();

    // Build the word-level polynomial W = Σ 2^bit * (mir_lit_poly - gate_lit_poly)
    let mut w = Polynomial::zero();
    let mut all_root_nodes = BTreeSet::new();

    for &(miter_idx, ref name, bit_idx) in group {
        // Find the matched pair for this diff gate
        // The diff gate name is "diff_output[X]" or "diff_next_state[X]"
        // The matched pair name is "output[X]" or "next_state[X]"
        let pair_name = name
            .strip_prefix("diff_")
            .unwrap_or(name);

        let pair = matched_pairs.iter().find(|(n, _, _)| n == pair_name);
        let (mir_lit, gate_lit) = match pair {
            Some((_, m, g)) => (*m, *g),
            None => {
                return AlgebraicResult::Unknown {
                    reason: "pair not found in matched_pairs".to_string(),
                    cone_size: 0,
                    terms_at_failure: 0,
                };
            }
        };

        let mir_poly = lit_to_poly(&mir_lit);
        let gate_poly = lit_to_poly(&gate_lit);
        let diff = mir_poly.sub(&gate_poly);
        let weight = 1i64 << bit_idx.min(62); // 2^bit, capped to avoid overflow
        let weighted = diff.scale(weight);
        w = w.add(&weighted);

        // Collect root nodes for cone computation
        if mir_lit.node.0 != 0 {
            all_root_nodes.insert(mir_lit.node.0);
        }
        if gate_lit.node.0 != 0 {
            all_root_nodes.insert(gate_lit.node.0);
        }
    }

    if w.is_zero() {
        return AlgebraicResult::Proven {
            substitutions: 0,
            time_ms: group_start.elapsed().as_millis() as u64,
        };
    }

    // Collect cone nodes in reverse topological order
    let cone_nodes = match collect_cone_nodes(miter, &all_root_nodes) {
        Some(nodes) => nodes,
        None => {
            return AlgebraicResult::Unknown {
                reason: format!("cone exceeds {} AND nodes", MAX_CONE_SIZE),
                cone_size: MAX_CONE_SIZE,
                terms_at_failure: w.term_count(),
            };
        }
    };

    let cone_size = cone_nodes.len();
    let deadline = group_start + std::time::Duration::from_secs(GROUP_TIMEOUT_SECS);

    // Backward rewrite: substitute AND gates from outputs toward inputs
    let mut substitutions = 0usize;
    for node_id in &cone_nodes {
        // Time check EVERY substitution
        if substitutions > 0 && std::time::Instant::now() >= deadline {
            return AlgebraicResult::Unknown {
                reason: format!("timeout after {}s", group_start.elapsed().as_secs()),
                cone_size,
                terms_at_failure: w.term_count(),
            };
        }

        // O(1) check if this variable appears in W at all
        if !w.contains_var(*node_id) {
            continue;
        }

        if let Some(AigNode::And { left, right }) = miter.nodes.get(*node_id as usize) {
            // AND(a, b) = a · b as a polynomial
            // Use WORKING_TERM_LIMIT for early bail in intermediate product
            let left_poly = lit_to_poly(left);
            let right_poly = lit_to_poly(right);
            let gate_poly = match left_poly.mul_with_limit(&right_poly, WORKING_TERM_LIMIT) {
                Some(p) => p,
                None => {
                    return AlgebraicResult::Unknown {
                        reason: format!("intermediate AND product exceeded {} term limit", WORKING_TERM_LIMIT),
                        cone_size,
                        terms_at_failure: w.term_count(),
                    };
                }
            };

            // Substitute: replace var(node_id) with gate_poly in W
            w = match w.substitute(*node_id, &gate_poly, deadline) {
                Some(p) => p,
                None => {
                    return AlgebraicResult::Unknown {
                        reason: format!("substitution exceeded term/time limit ({} subs, {} terms)",
                            substitutions, w.term_count()),
                        cone_size,
                        terms_at_failure: w.term_count(),
                    };
                }
            };

            substitutions += 1;

            if w.is_zero() {
                return AlgebraicResult::Proven {
                    substitutions,
                    time_ms: group_start.elapsed().as_millis() as u64,
                };
            }

            // Working term limit — if polynomial is too large, this isn't a clean multiplier
            if w.term_count() > WORKING_TERM_LIMIT {
                return AlgebraicResult::Unknown {
                    reason: format!("working term limit {} exceeded ({} terms after {} subs)",
                        WORKING_TERM_LIMIT, w.term_count(), substitutions),
                    cone_size,
                    terms_at_failure: w.term_count(),
                };
            }
        }
    }

    if w.is_zero() {
        AlgebraicResult::Proven {
            substitutions,
            time_ms: group_start.elapsed().as_millis() as u64,
        }
    } else {
        // Count remaining variables in the polynomial
        let remaining_vars = w.active_vars.len();
        AlgebraicResult::Unknown {
            reason: format!("non-zero remainder after all substitutions ({} terms, {} variables remaining)",
                w.term_count(), remaining_vars),
            cone_size,
            terms_at_failure: w.term_count(),
        }
    }
}

/// Entry point: verify unresolved diff gates using algebraic polynomial rewriting.
///
/// Returns structured results with per-group diagnostics and the names of proven gates.
///
/// # Arguments
/// - `miter`: The miter AIG (from build_sequential_miter)
/// - `matched_pairs`: Pre-XOR literal pairs (name, mir_lit, gate_lit) from miter construction
/// - `unresolved`: Gates that SAT couldn't resolve: (miter_output_index, diff_gate_name)
pub fn verify_unresolved_algebraic(
    miter: &Aig,
    matched_pairs: &[(String, AigLit, AigLit)],
    unresolved: &[(usize, String)],
) -> AlgebraicVerificationResult {
    if unresolved.is_empty() {
        return AlgebraicVerificationResult {
            proven_names: Vec::new(),
            group_results: Vec::new(),
            total_time_ms: 0,
        };
    }

    let start = std::time::Instant::now();
    let groups = group_gates_by_signal(unresolved);
    let total_groups = groups.len();


    // Process groups in parallel — each group is independent
    let groups_vec: Vec<(String, Vec<(usize, String, u32)>)> = groups.into_iter().collect();
    let raw_results: Vec<(String, Vec<(usize, String, u32)>, AlgebraicResult)> = groups_vec
        .par_iter()
        .map(|(base_name, group)| {
            let result = verify_word_level_group(miter, group, matched_pairs);
            (base_name.clone(), group.clone(), result)
        })
        .collect();

    // Collect results sequentially for deterministic output
    let mut proven_names: Vec<String> = Vec::new();
    let mut group_results: Vec<AlgebraicGroupReport> = Vec::new();

    for (base_name, group, result) in &raw_results {
        let num_bits = group.len();
        match result {
            AlgebraicResult::Proven { substitutions, time_ms } => {
                for (_, name, _) in group {
                    proven_names.push(name.clone());
                }
                group_results.push(AlgebraicGroupReport {
                    name: base_name.clone(),
                    num_bits,
                    status: AlgebraicGroupStatus::Proven {
                        substitutions: *substitutions,
                        time_ms: *time_ms,
                    },
                });
            }
            AlgebraicResult::Unknown { reason, cone_size, terms_at_failure } => {
                group_results.push(AlgebraicGroupReport {
                    name: base_name.clone(),
                    num_bits,
                    status: AlgebraicGroupStatus::Unknown {
                        reason: reason.clone(),
                        cone_size: *cone_size,
                        terms_at_failure: *terms_at_failure,
                    },
                });
            }
        }
    }

    let elapsed = start.elapsed().as_millis();

    AlgebraicVerificationResult {
        proven_names,
        group_results,
        total_time_ms: elapsed as u64,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_polynomial_basic() {
        let zero = Polynomial::zero();
        assert!(zero.is_zero());

        let one = Polynomial::constant(1);
        assert!(!one.is_zero());
        assert_eq!(one.term_count(), 1);

        let x = Polynomial::var(1);
        let y = Polynomial::var(2);

        // x + y
        let sum = x.add(&y);
        assert_eq!(sum.term_count(), 2);

        // x - x = 0
        let diff = x.sub(&x);
        assert!(diff.is_zero());
    }

    #[test]
    fn test_polynomial_mul_boolean() {
        let x = Polynomial::var(1);

        // x * x = x (boolean constraint)
        let x_sq = x.mul(&x).unwrap();
        assert_eq!(x_sq.term_count(), 1);
        assert_eq!(x_sq.terms.get(&vec![1u32]), Some(&1i64));

        // x * y
        let y = Polynomial::var(2);
        let xy = x.mul(&y).unwrap();
        assert_eq!(xy.term_count(), 1);
        assert_eq!(xy.terms.get(&vec![1u32, 2u32]), Some(&1i64));
    }

    #[test]
    fn test_polynomial_substitute() {
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
        // W = x, substitute x → y*z
        let x = Polynomial::var(1);
        let y = Polynomial::var(2);
        let z = Polynomial::var(3);
        let yz = y.mul(&z).unwrap();

        let result = x.substitute(1, &yz, deadline).unwrap();
        assert_eq!(result.term_count(), 1);
        assert_eq!(result.terms.get(&vec![2u32, 3u32]), Some(&1i64));
    }

    #[test]
    fn test_polynomial_and_gate_rewrite() {
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
        // Verify that AND gate rewriting works:
        // If g = AND(a, b), then substituting g → a·b should work
        let a = Polynomial::var(10);
        let b = Polynomial::var(11);
        let g = Polynomial::var(20);

        // W = g - a·b (should be 0 after substitution)
        let ab = a.mul(&b).unwrap();
        let w = g.sub(&ab);

        // Substitute g → a·b
        let result = w.substitute(20, &ab, deadline).unwrap();
        assert!(result.is_zero());
    }

    #[test]
    fn test_polynomial_inverter() {
        // NOT(a) = 1 - a
        let a = Polynomial::var(1);
        let not_a = Polynomial::constant(1).sub(&a);

        // a + (1-a) = 1
        let sum = a.add(&not_a);
        assert_eq!(sum.term_count(), 1);
        assert_eq!(sum.terms.get(&Vec::<u32>::new()), Some(&1i64));

        // a * (1-a) = a - a² = a - a = 0
        let product = a.mul(&not_a).unwrap();
        assert!(product.is_zero());
    }

    #[test]
    fn test_contains_var() {
        let x = Polynomial::var(5);
        let y = Polynomial::var(10);
        let sum = x.add(&y);

        assert!(sum.contains_var(5));
        assert!(sum.contains_var(10));
        assert!(!sum.contains_var(7));

        let zero = Polynomial::zero();
        assert!(!zero.contains_var(5));

        let c = Polynomial::constant(42);
        assert!(!c.contains_var(0));
    }

    #[test]
    fn test_group_gates_by_signal() {
        let unresolved = vec![
            (0, "diff_output[top.vl.output_reg[0]]".to_string()),
            (1, "diff_output[top.vl.output_reg[1]]".to_string()),
            (2, "diff_output[top.vl.output_reg[2]]".to_string()),
            (3, "diff_next_state[top.vl.int_accum[0]]".to_string()),
            (4, "diff_next_state[top.vl.int_accum[1]]".to_string()),
            (5, "diff_output[top.vl.saturated]".to_string()),
        ];

        let groups = group_gates_by_signal(&unresolved);

        assert_eq!(groups.len(), 3);
        assert_eq!(groups["top.vl.output_reg"].len(), 3);
        assert_eq!(groups["top.vl.int_accum"].len(), 2);
        assert_eq!(groups["top.vl.saturated"].len(), 1);

        // Check bit indices
        let or_group = &groups["top.vl.output_reg"];
        assert_eq!(or_group[0].2, 0); // bit 0
        assert_eq!(or_group[1].2, 1); // bit 1
        assert_eq!(or_group[2].2, 2); // bit 2
    }

    #[test]
    fn test_trivial_miter_equivalence() {
        // Build a trivial miter where both sides are the same: mir_lit == gate_lit
        // The diff should be zero without any rewriting
        use crate::equivalence::AigNodeId;

        let lit = AigLit::new(AigNodeId(0), false); // constant false
        let matched_pairs = vec![
            ("output[x]".to_string(), lit, lit),
        ];
        let unresolved = vec![
            (0, "diff_output[x]".to_string()),
        ];

        let miter = Aig::new();
        let result = verify_unresolved_algebraic(&miter, &matched_pairs, &unresolved);
        assert_eq!(result.proven_names.len(), 1);
    }

    #[test]
    fn test_simple_and_gate_equivalence() {
        // Build a simple miter where both sides compute AND(a, b) but via different node IDs
        use crate::equivalence::AigNodeId;

        let mut miter = Aig::new();

        // Node 0: constant false (always present)
        // Node 1: input a
        let a_lit = miter.add_input("a".to_string());
        // Node 2: input b
        let b_lit = miter.add_input("b".to_string());
        // Node 3: AND(a, b) — "MIR side"
        let mir_out = miter.add_and(a_lit, b_lit);
        // Node 4: AND(a, b) — "gate side" (same function, different node)
        let gate_out = miter.add_and(a_lit, b_lit);

        let matched_pairs = vec![
            ("output[x[0]]".to_string(), mir_out, gate_out),
        ];
        let unresolved = vec![
            (0, "diff_output[x[0]]".to_string()),
        ];

        let result = verify_unresolved_algebraic(&miter, &matched_pairs, &unresolved);
        assert_eq!(result.proven_names.len(), 1);
    }

    #[test]
    fn test_mul_with_limit() {
        let x = Polynomial::var(1);
        let y = Polynomial::var(2);

        // Normal mul works fine
        let xy = x.mul_with_limit(&y, 100).unwrap();
        assert_eq!(xy.term_count(), 1);

        // Zero limit causes bail
        let result = x.mul_with_limit(&y, 0);
        assert!(result.is_none());
    }
}
