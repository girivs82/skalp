//! AIG Refactoring Pass
//!
//! This pass performs cone-based refactoring, which works on larger
//! logic cones than the rewrite pass. It collects the leaves of a
//! cone and rebuilds it using algebraic factorization.
//!
//! # Algorithm
//!
//! For each node with multiple fanins:
//! 1. Collect all leaves up to a depth limit
//! 2. Compute the Boolean function of the cone
//! 3. Factor the function using algebraic methods (ISOP decomposition)
//! 4. Rebuild with the factored form if smaller
//!
//! # References
//!
//! - Mishchenko, A., Chatterjee, S., & Brayton, R. (2006). DAG-aware AIG rewriting.
//! - Brayton, R. K., & McMullen, C. (1982). The decomposition and factorization of Boolean expressions.

use super::{Pass, PassResult};
use crate::synth::{Aig, AigLit, AigNode, AigNodeId};
use std::collections::{HashMap, HashSet};

/// Maximum depth for cone collection
const MAX_CONE_DEPTH: usize = 10;

/// Maximum number of leaves for refactoring
const MAX_CONE_LEAVES: usize = 12;

/// Maximum leaves for truth table computation (2^6 = 64 bits)
const MAX_TT_LEAVES: usize = 6;

/// AIG refactoring pass
pub struct Refactor {
    /// Maximum cone depth
    max_depth: usize,
    /// Maximum cone leaves
    max_leaves: usize,
    /// Zero-cost mode: allow refactoring with savings >= 0
    zero_cost: bool,
    /// Number of cones refactored
    refactored_count: usize,
    /// Total nodes saved
    total_savings: i32,
}

impl Refactor {
    /// Create a new refactoring pass with default parameters
    pub fn new() -> Self {
        Self {
            max_depth: MAX_CONE_DEPTH,
            max_leaves: MAX_CONE_LEAVES,
            zero_cost: false,
            refactored_count: 0,
            total_savings: 0,
        }
    }

    /// Create a refactoring pass with zero-cost mode enabled
    /// (equivalent to ABC's `refactor -z`)
    pub fn zero_cost() -> Self {
        Self {
            max_depth: MAX_CONE_DEPTH,
            max_leaves: MAX_CONE_LEAVES,
            zero_cost: true,
            refactored_count: 0,
            total_savings: 0,
        }
    }

    /// Create a refactoring pass with custom parameters
    pub fn with_params(max_depth: usize, max_leaves: usize) -> Self {
        Self {
            max_depth,
            max_leaves,
            zero_cost: false,
            refactored_count: 0,
            total_savings: 0,
        }
    }

    /// Collect the leaves of a cone rooted at the given node
    fn collect_cone(
        &self,
        aig: &Aig,
        root: AigNodeId,
        fanout_counts: &HashMap<AigNodeId, usize>,
    ) -> ConeInfo {
        let mut leaves = Vec::new();
        let mut internal = HashSet::new();
        let mut internal_ordered = Vec::new();
        let mut stack = vec![(root, 0usize)];
        let mut visited = HashSet::new();
        let mut incomplete = false;

        while let Some((node, depth)) = stack.pop() {
            if visited.contains(&node) {
                continue;
            }
            visited.insert(node);

            // Check if this should be a leaf
            let (is_leaf, forced_by_limit) = if node == root {
                (false, false)
            } else if depth >= self.max_depth || leaves.len() >= self.max_leaves {
                (true, true)
            } else {
                match aig.get_node(node) {
                    Some(AigNode::And { .. }) => {
                        // Multi-fanout nodes become leaves
                        let fanouts = fanout_counts.get(&node).copied().unwrap_or(0);
                        (fanouts > 1, false)
                    }
                    Some(AigNode::Input { .. })
                    | Some(AigNode::Latch { .. })
                    | Some(AigNode::Barrier { .. }) => (true, false),
                    Some(AigNode::Const) => (true, false),
                    None => (true, false),
                }
            };

            if is_leaf {
                if !leaves.contains(&node) {
                    leaves.push(node);
                }
                // If we forced this node to be a leaf due to limits, and it's an AND node,
                // the cone is incomplete because we haven't explored its fanins
                if forced_by_limit {
                    if let Some(AigNode::And { .. }) = aig.get_node(node) {
                        incomplete = true;
                    }
                }
            } else {
                internal.insert(node);
                internal_ordered.push(node);
                if let Some(n) = aig.get_node(node) {
                    for fanin in n.fanins() {
                        stack.push((fanin.node, depth + 1));
                    }
                }
            }
        }

        ConeInfo {
            root,
            leaves,
            internal_nodes: internal,
            internal_ordered,
            incomplete,
        }
    }

    /// Compute the truth table of a cone
    fn compute_cone_truth_table(&self, aig: &Aig, cone: &ConeInfo) -> Option<u64> {
        if cone.leaves.len() > MAX_TT_LEAVES {
            return None;
        }

        let num_inputs = cone.leaves.len();
        let num_rows = 1usize << num_inputs;

        // Create leaf to input index mapping
        let leaf_to_idx: HashMap<AigNodeId, usize> = cone
            .leaves
            .iter()
            .enumerate()
            .map(|(i, &n)| (n, i))
            .collect();

        let mut truth_table = 0u64;

        // Evaluate for each input combination
        for row in 0..num_rows {
            let result =
                self.evaluate_cone(aig, cone.root, row, &leaf_to_idx, &cone.internal_nodes);
            if result {
                truth_table |= 1u64 << row;
            }
        }

        Some(truth_table)
    }

    /// Evaluate the cone for a specific input assignment
    fn evaluate_cone(
        &self,
        aig: &Aig,
        node: AigNodeId,
        input_assignment: usize,
        leaf_to_idx: &HashMap<AigNodeId, usize>,
        internal: &HashSet<AigNodeId>,
    ) -> bool {
        self.evaluate_lit(
            aig,
            AigLit::new(node),
            input_assignment,
            leaf_to_idx,
            internal,
            &mut HashMap::new(),
        )
    }

    /// Recursively evaluate a literal
    #[allow(clippy::only_used_in_recursion)]
    fn evaluate_lit(
        &self,
        aig: &Aig,
        lit: AigLit,
        input_assignment: usize,
        leaf_to_idx: &HashMap<AigNodeId, usize>,
        internal: &HashSet<AigNodeId>,
        cache: &mut HashMap<AigNodeId, bool>,
    ) -> bool {
        // Check if it's a leaf (input)
        if let Some(&idx) = leaf_to_idx.get(&lit.node) {
            let val = (input_assignment >> idx) & 1 == 1;
            return if lit.inverted { !val } else { val };
        }

        // Check cache
        if let Some(&cached) = cache.get(&lit.node) {
            return if lit.inverted { !cached } else { cached };
        }

        // Must be an internal node
        let val = match aig.get_node(lit.node) {
            Some(AigNode::And { left, right, .. }) => {
                let left_val =
                    self.evaluate_lit(aig, *left, input_assignment, leaf_to_idx, internal, cache);
                let right_val =
                    self.evaluate_lit(aig, *right, input_assignment, leaf_to_idx, internal, cache);
                left_val && right_val
            }
            Some(AigNode::Const) => false, // Constant 0
            // If we reach Input/Latch/Barrier here, the cone was incomplete.
            // This should be caught by the cone.incomplete flag check in try_refactor_cone.
            _ => false,
        };

        cache.insert(lit.node, val);
        if lit.inverted {
            !val
        } else {
            val
        }
    }

    /// Factor a truth table using ISOP (Irredundant Sum of Products) decomposition
    /// Returns the factored form as a list of AIG gates and the result literal
    fn factor_truth_table(&self, tt: u64, num_inputs: usize) -> Option<FactoredForm> {
        if num_inputs > MAX_TT_LEAVES {
            return None;
        }

        let num_rows = 1usize << num_inputs;
        // When num_rows = 64, we need u64::MAX as the mask
        // (1 << 64 overflows, wrapping_shl wraps to 0 shift, giving mask=0)
        let mask = if num_rows >= 64 {
            u64::MAX
        } else {
            (1u64 << num_rows) - 1
        };
        let tt = tt & mask;

        // Handle constants
        if tt == 0 {
            return Some(FactoredForm {
                gates: vec![],
                result_lit: 0, // Constant 0
                is_constant: Some(false),
            });
        }
        if tt == mask {
            return Some(FactoredForm {
                gates: vec![],
                result_lit: 1, // Constant 1
                is_constant: Some(true),
            });
        }

        // Check for single variable
        for i in 0..num_inputs {
            let var_tt = variable_truth_table(i, num_inputs);
            if tt == var_tt {
                return Some(FactoredForm {
                    gates: vec![],
                    result_lit: (i as u8) * 2,
                    is_constant: None,
                });
            }
            if tt == (var_tt ^ mask) {
                return Some(FactoredForm {
                    gates: vec![],
                    result_lit: (i as u8) * 2 + 1,
                    is_constant: None,
                });
            }
        }

        // Try Shannon decomposition for larger functions
        if num_inputs >= 3 {
            if let Some(form) = self.factor_shannon(tt, num_inputs) {
                return Some(form);
            }
        }

        // Fall back to SOP factorization
        self.factor_sop(tt, num_inputs)
    }

    /// Factor using Shannon decomposition: f = x*f_x + x'*f_x'
    fn factor_shannon(&self, tt: u64, num_inputs: usize) -> Option<FactoredForm> {
        let num_rows = 1usize << num_inputs;
        let mask = if num_rows >= 64 {
            u64::MAX
        } else {
            (1u64 << num_rows) - 1
        };

        // Find the best variable to split on (one that creates smallest subfunctions)
        let mut best_var = 0;
        let mut best_cost = usize::MAX;

        for var in 0..num_inputs {
            let (cofactor_0, cofactor_1) = compute_cofactors(tt, var, num_inputs);

            // Estimate cost as number of 1s in each cofactor
            let cost = (cofactor_0 & mask).count_ones() + (cofactor_1 & mask).count_ones();
            if (cost as usize) < best_cost {
                best_cost = cost as usize;
                best_var = var;
            }
        }

        let (cofactor_0, cofactor_1) = compute_cofactors(tt, best_var, num_inputs);
        let cofactor_0 = cofactor_0 & mask;
        let cofactor_1 = cofactor_1 & mask;

        // Check for simple cases
        if cofactor_0 == 0 {
            // f = x * f_x
            if let Some(sub) = self.factor_truth_table(cofactor_1, num_inputs) {
                let mut gates = sub.gates;
                let sub_result = remap_result_lit(sub.result_lit, num_inputs, gates.len());

                // AND with variable
                let var_lit = (best_var as u8) * 2;
                gates.push((var_lit, sub_result));
                let result = ((num_inputs + gates.len() - 1) as u8) * 2;

                return Some(FactoredForm {
                    gates,
                    result_lit: result,
                    is_constant: None,
                });
            }
        }

        if cofactor_1 == 0 {
            // f = x' * f_x'
            if let Some(sub) = self.factor_truth_table(cofactor_0, num_inputs) {
                let mut gates = sub.gates;
                let sub_result = remap_result_lit(sub.result_lit, num_inputs, gates.len());

                // AND with negated variable
                let var_lit = (best_var as u8) * 2 + 1;
                gates.push((var_lit, sub_result));
                let result = ((num_inputs + gates.len() - 1) as u8) * 2;

                return Some(FactoredForm {
                    gates,
                    result_lit: result,
                    is_constant: None,
                });
            }
        }

        if cofactor_0 == mask {
            // f = x' + f_x = !(x * !f_x)
            let not_f1 = cofactor_1 ^ mask;
            if let Some(sub) = self.factor_truth_table(not_f1, num_inputs) {
                let mut gates = sub.gates;
                let sub_result = remap_result_lit(sub.result_lit, num_inputs, gates.len());

                // x AND !f_x
                let var_lit = (best_var as u8) * 2;
                gates.push((var_lit, sub_result));
                let and_result = ((num_inputs + gates.len() - 1) as u8) * 2;

                // Result is NOT of that
                return Some(FactoredForm {
                    gates,
                    result_lit: and_result + 1,
                    is_constant: None,
                });
            }
        }

        if cofactor_1 == mask {
            // f = x + f_x' = !(x' * !f_x')
            let not_f0 = cofactor_0 ^ mask;
            if let Some(sub) = self.factor_truth_table(not_f0, num_inputs) {
                let mut gates = sub.gates;
                let sub_result = remap_result_lit(sub.result_lit, num_inputs, gates.len());

                // x' AND !f_x'
                let var_lit = (best_var as u8) * 2 + 1;
                gates.push((var_lit, sub_result));
                let and_result = ((num_inputs + gates.len() - 1) as u8) * 2;

                return Some(FactoredForm {
                    gates,
                    result_lit: and_result + 1,
                    is_constant: None,
                });
            }
        }

        // General case: f = x*f_x + x'*f_x' = !(!x*f_x) * !(x'*f_x'))
        // This is MUX: implemented as !(!(x*f_x) * !(!x*f_x'))
        None // Fall back to SOP for complex cases
    }

    /// Factor using Sum-of-Products with minterm combination
    fn factor_sop(&self, tt: u64, num_inputs: usize) -> Option<FactoredForm> {
        let num_rows = 1usize << num_inputs;
        let mask = if num_rows >= 64 {
            u64::MAX
        } else {
            (1u64 << num_rows) - 1
        };
        let tt = tt & mask;

        // Count ones - if more than half, compute complement
        let ones = tt.count_ones() as usize;
        let use_complement = ones > num_rows / 2;
        let working_tt = if use_complement { tt ^ mask } else { tt };

        // Collect minterms
        let mut minterms: Vec<usize> = Vec::new();
        for row in 0..num_rows {
            if (working_tt >> row) & 1 == 1 {
                minterms.push(row);
            }
        }

        if minterms.is_empty() {
            return Some(FactoredForm {
                gates: vec![],
                result_lit: if use_complement { 1 } else { 0 },
                is_constant: Some(!use_complement),
            });
        }

        if minterms.len() == 1 {
            // Single minterm - just AND all literals
            return self.build_single_minterm(minterms[0], num_inputs, use_complement);
        }

        // Build product terms and OR them
        let mut gates: Vec<(u8, u8)> = Vec::new();
        let mut product_lits: Vec<u8> = Vec::new();

        for &minterm in &minterms {
            let product_lit = self.build_product_term(&mut gates, minterm, num_inputs);
            product_lits.push(product_lit);
        }

        // OR all products: a + b = !(!a * !b)
        let result_lit = self.or_products(&mut gates, &product_lits, num_inputs);

        // If we used complement, invert the result
        let final_result = if use_complement {
            result_lit ^ 1
        } else {
            result_lit
        };

        Some(FactoredForm {
            gates,
            result_lit: final_result,
            is_constant: None,
        })
    }

    /// Build a single minterm as a product of literals
    fn build_single_minterm(
        &self,
        minterm: usize,
        num_inputs: usize,
        complement: bool,
    ) -> Option<FactoredForm> {
        let mut gates: Vec<(u8, u8)> = Vec::new();
        let mut current_lit: Option<u8> = None;

        for i in 0..num_inputs {
            let bit = (minterm >> i) & 1;
            let var_lit = (i as u8) * 2 + if bit == 0 { 1 } else { 0 };

            if let Some(prev) = current_lit {
                gates.push((prev, var_lit));
                current_lit = Some(((num_inputs + gates.len() - 1) as u8) * 2);
            } else {
                current_lit = Some(var_lit);
            }
        }

        let result = current_lit.unwrap_or(1); // Default to constant 1 if no inputs
        let final_result = if complement { result ^ 1 } else { result };

        Some(FactoredForm {
            gates,
            result_lit: final_result,
            is_constant: None,
        })
    }

    /// Build a product term (AND of literals) for a minterm
    fn build_product_term(
        &self,
        gates: &mut Vec<(u8, u8)>,
        minterm: usize,
        num_inputs: usize,
    ) -> u8 {
        let mut lits: Vec<u8> = Vec::new();

        for i in 0..num_inputs {
            let bit = (minterm >> i) & 1;
            // If bit is 1, use variable; if 0, use complement
            let lit = (i as u8) * 2 + if bit == 0 { 1 } else { 0 };
            lits.push(lit);
        }

        self.and_lits(gates, &lits, num_inputs)
    }

    /// AND multiple literals together
    fn and_lits(&self, gates: &mut Vec<(u8, u8)>, lits: &[u8], num_inputs: usize) -> u8 {
        if lits.is_empty() {
            return 1; // Constant 1
        }
        if lits.len() == 1 {
            return lits[0];
        }

        // Build balanced tree of ANDs
        let mut current_lits = lits.to_vec();

        while current_lits.len() > 1 {
            let mut next_lits = Vec::new();

            for i in (0..current_lits.len()).step_by(2) {
                if i + 1 < current_lits.len() {
                    let left = current_lits[i];
                    let right = current_lits[i + 1];
                    gates.push((left, right));
                    let new_lit = ((num_inputs + gates.len() - 1) as u8) * 2;
                    next_lits.push(new_lit);
                } else {
                    next_lits.push(current_lits[i]);
                }
            }

            current_lits = next_lits;
        }

        current_lits[0]
    }

    /// OR multiple literals together: a + b = !(!a * !b)
    fn or_products(&self, gates: &mut Vec<(u8, u8)>, lits: &[u8], num_inputs: usize) -> u8 {
        if lits.is_empty() {
            return 0; // Constant 0
        }
        if lits.len() == 1 {
            return lits[0];
        }

        // OR is implemented as NAND of complements
        let neg_lits: Vec<u8> = lits.iter().map(|&l| l ^ 1).collect();
        let and_result = self.and_lits(gates, &neg_lits, num_inputs);
        and_result ^ 1 // Invert the result
    }

    /// Build the factored form in the AIG
    fn build_factored_form(
        &self,
        aig: &mut Aig,
        cone: &ConeInfo,
        form: &FactoredForm,
    ) -> Option<AigLit> {
        let num_inputs = cone.leaves.len();

        // Handle constants
        if let Some(is_one) = form.is_constant {
            return Some(if is_one {
                AigLit {
                    node: AigNodeId(0),
                    inverted: true,
                } // Constant 1
            } else {
                AigLit {
                    node: AigNodeId(0),
                    inverted: false,
                } // Constant 0
            });
        }

        // Handle direct variable reference
        if form.gates.is_empty() {
            let idx = (form.result_lit / 2) as usize;
            let inv = form.result_lit & 1 == 1;

            if idx < num_inputs {
                return Some(AigLit {
                    node: cone.leaves[idx],
                    inverted: inv,
                });
            }
            return None;
        }

        // Build the gates
        let mut node_lits: Vec<AigLit> = cone.leaves.iter().map(|&n| AigLit::new(n)).collect();

        for &(left_encoded, right_encoded) in &form.gates {
            let left_idx = (left_encoded / 2) as usize;
            let left_inv = left_encoded & 1 == 1;
            let right_idx = (right_encoded / 2) as usize;
            let right_inv = right_encoded & 1 == 1;

            if left_idx >= node_lits.len() || right_idx >= node_lits.len() {
                return None;
            }

            let left_lit = AigLit {
                node: node_lits[left_idx].node,
                inverted: node_lits[left_idx].inverted ^ left_inv,
            };
            let right_lit = AigLit {
                node: node_lits[right_idx].node,
                inverted: node_lits[right_idx].inverted ^ right_inv,
            };

            let new_lit = aig.add_and(left_lit, right_lit);
            node_lits.push(new_lit);
        }

        // Get the result
        let result_idx = (form.result_lit / 2) as usize;
        let result_inv = form.result_lit & 1 == 1;

        if result_idx < node_lits.len() {
            Some(AigLit {
                node: node_lits[result_idx].node,
                inverted: node_lits[result_idx].inverted ^ result_inv,
            })
        } else {
            None
        }
    }

    /// Try to refactor a cone and return the potential savings
    fn try_refactor_cone(&self, aig: &Aig, cone: &ConeInfo) -> Option<(FactoredForm, i32)> {
        // Skip small cones
        if cone.internal_nodes.len() <= 2 {
            return None;
        }

        // Skip incomplete cones (hit max_leaves/max_depth limits with unexplored AND nodes)
        // These would produce incorrect truth tables
        if cone.incomplete {
            return None;
        }

        // Safety check: verify all leaves are valid (either Input/Latch/Barrier or multi-fanout AND)
        // If we find a leaf that is a single-fanout AND, the cone is invalid
        for &leaf_id in &cone.leaves {
            if let Some(node) = aig.get_node(leaf_id) {
                match node {
                    AigNode::Input { .. }
                    | AigNode::Latch { .. }
                    | AigNode::Barrier { .. }
                    | AigNode::Const => {
                        // These are valid leaves
                    }
                    AigNode::And { .. } => {
                        // AND nodes are valid leaves only if they're multi-fanout
                        // (verified during cone collection) - OK
                    }
                }
            } else {
                // Invalid node reference - skip this cone
                return None;
            }
        }

        // Compute truth table
        let tt = self.compute_cone_truth_table(aig, cone)?;

        // Factor the truth table
        let form = self.factor_truth_table(tt, cone.leaves.len())?;

        // Calculate savings: original gates - new gates
        let original_gates = cone.internal_nodes.len() as i32;
        let new_gates = form.gates.len() as i32;
        let savings = original_gates - new_gates;

        // Accept based on mode
        let accept = if self.zero_cost {
            savings >= 0
        } else {
            savings > 0
        };

        if accept {
            Some((form, savings))
        } else {
            None
        }
    }
}

/// Information about a logic cone
#[derive(Debug, Clone)]
struct ConeInfo {
    root: AigNodeId,
    leaves: Vec<AigNodeId>,
    internal_nodes: HashSet<AigNodeId>,
    internal_ordered: Vec<AigNodeId>,
    /// True if the cone is incomplete (hit max_leaves while AND nodes were unexplored)
    incomplete: bool,
}

/// A factored form representation
#[derive(Debug, Clone)]
struct FactoredForm {
    /// Gates as (left_lit, right_lit) pairs
    gates: Vec<(u8, u8)>,
    /// Result literal
    result_lit: u8,
    /// If Some(true), result is constant 1; if Some(false), constant 0
    is_constant: Option<bool>,
}

/// Compute fanout counts for all nodes
fn compute_fanout_counts(aig: &Aig) -> HashMap<AigNodeId, usize> {
    let mut counts: HashMap<AigNodeId, usize> = HashMap::new();

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

/// Compute truth table for a single variable
fn variable_truth_table(var: usize, num_inputs: usize) -> u64 {
    let num_rows = 1usize << num_inputs;
    let mut tt = 0u64;
    for row in 0..num_rows {
        if (row >> var) & 1 == 1 {
            tt |= 1 << row;
        }
    }
    tt
}

/// Compute cofactors of a function with respect to a variable
fn compute_cofactors(tt: u64, var: usize, num_inputs: usize) -> (u64, u64) {
    let num_rows = 1usize << num_inputs;
    let mut cofactor_0 = 0u64;
    let mut cofactor_1 = 0u64;

    for row in 0..num_rows {
        let val = (tt >> row) & 1;
        let var_val = (row >> var) & 1;

        // Row with variable set to 0
        let row_0 = row & !(1 << var);
        // Row with variable set to 1
        let row_1 = row | (1 << var);

        if var_val == 0 {
            cofactor_0 |= val << row;
            cofactor_0 |= val << row_1;
        } else {
            cofactor_1 |= val << row;
            cofactor_1 |= val << row_0;
        }
    }

    (cofactor_0, cofactor_1)
}

/// Remap a result literal after adding gates
fn remap_result_lit(lit: u8, num_inputs: usize, gates_added: usize) -> u8 {
    let idx = (lit / 2) as usize;
    let inv = lit & 1;

    if idx < num_inputs {
        lit // Input literal, no change
    } else {
        // Gate reference - need to remap
        ((idx + gates_added) as u8) * 2 + inv
    }
}

impl Default for Refactor {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for Refactor {
    fn name(&self) -> &str {
        if self.zero_cost {
            "refactor_z"
        } else {
            "refactor"
        }
    }

    fn run(&mut self, aig: &mut Aig) -> PassResult {
        let mut result = PassResult::new(self.name());
        result.record_before(aig);

        self.refactored_count = 0;
        self.total_savings = 0;

        // Compute fanout counts
        let fanout_counts = compute_fanout_counts(aig);

        // Collect nodes to process (in topological order)
        let nodes: Vec<AigNodeId> = aig
            .iter_nodes()
            .filter_map(|(id, node)| {
                if matches!(node, AigNode::And { .. }) {
                    Some(id)
                } else {
                    None
                }
            })
            .collect();

        // Find refactoring opportunities
        let mut substitutions: HashMap<AigNodeId, AigLit> = HashMap::new();

        for node_id in nodes {
            // Skip if already substituted
            if substitutions.contains_key(&node_id) {
                continue;
            }

            let cone = self.collect_cone(aig, node_id, &fanout_counts);

            // Skip cones with too many leaves for truth table
            if cone.leaves.len() > MAX_TT_LEAVES {
                continue;
            }

            // Try to refactor
            if let Some((form, savings)) = self.try_refactor_cone(aig, &cone) {
                // Build the new structure
                if let Some(new_lit) = self.build_factored_form(aig, &cone, &form) {
                    // Only substitute if we got a different node
                    if new_lit.node != node_id {
                        substitutions.insert(node_id, new_lit);
                        self.refactored_count += 1;
                        self.total_savings += savings;
                    }
                }
            }
        }

        // Apply substitutions
        if !substitutions.is_empty() {
            aig.apply_substitutions(&substitutions);

            // Run DCE to clean up
            let mut dce = super::Dce::new();
            dce.run(aig);
        }

        result.record_after(aig);
        result.add_extra("cones_refactored", &self.refactored_count.to_string());
        result.add_extra("nodes_saved", &self.total_savings.to_string());
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_refactor_creation() {
        let pass = Refactor::new();
        assert_eq!(pass.name(), "refactor");

        let pass_z = Refactor::zero_cost();
        assert_eq!(pass_z.name(), "refactor_z");
    }

    #[test]
    fn test_refactor_simple() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        aig.add_output("y".to_string(), ab);

        let mut pass = Refactor::new();
        let result = pass.run(&mut aig);

        // Should complete without error
        assert!(result.ands_after >= 1);
    }

    #[test]
    fn test_collect_cone() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);
        let c = aig.add_input("c".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let abc = aig.add_and(ab, AigLit::new(c));
        aig.add_output("y".to_string(), abc);

        let fanout_counts = compute_fanout_counts(&aig);
        let refactor = Refactor::new();

        let cone = refactor.collect_cone(&aig, abc.node, &fanout_counts);

        // Leaves should be a, b, c
        assert_eq!(cone.leaves.len(), 3);
        assert!(cone.leaves.contains(&a));
        assert!(cone.leaves.contains(&b));
        assert!(cone.leaves.contains(&c));

        // Internal nodes: ab and abc
        assert_eq!(cone.internal_nodes.len(), 2);
    }

    #[test]
    fn test_variable_truth_table() {
        // Variable 0 in 2-input: 0b1010
        assert_eq!(variable_truth_table(0, 2), 0b1010);
        // Variable 1 in 2-input: 0b1100
        assert_eq!(variable_truth_table(1, 2), 0b1100);
    }

    #[test]
    fn test_factor_constant() {
        let refactor = Refactor::new();

        // Constant 0
        let form = refactor.factor_truth_table(0, 2).unwrap();
        assert!(form.gates.is_empty());
        assert_eq!(form.is_constant, Some(false));

        // Constant 1
        let form = refactor.factor_truth_table(0xF, 2).unwrap();
        assert!(form.gates.is_empty());
        assert_eq!(form.is_constant, Some(true));
    }

    #[test]
    fn test_factor_variable() {
        let refactor = Refactor::new();

        // Variable a (0b1010)
        let form = refactor.factor_truth_table(0b1010, 2).unwrap();
        assert!(form.gates.is_empty());
        assert_eq!(form.result_lit, 0); // Variable 0, not inverted

        // Variable !a (0b0101)
        let form = refactor.factor_truth_table(0b0101, 2).unwrap();
        assert!(form.gates.is_empty());
        assert_eq!(form.result_lit, 1); // Variable 0, inverted
    }

    #[test]
    fn test_factor_and() {
        let refactor = Refactor::new();

        // a AND b = 0b1000
        let form = refactor.factor_truth_table(0b1000, 2).unwrap();
        assert_eq!(form.gates.len(), 1);
    }

    #[test]
    fn test_collect_cone_with_shared() {
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        let ab = aig.add_and(AigLit::new(a), AigLit::new(b));
        let aab = aig.add_and(ab, AigLit::new(a));

        // ab is used twice, so output both
        aig.add_output("y1".to_string(), ab);
        aig.add_output("y2".to_string(), aab);

        let fanout_counts = compute_fanout_counts(&aig);
        let refactor = Refactor::new();

        let cone = refactor.collect_cone(&aig, aab.node, &fanout_counts);

        // ab should be a leaf (multi-fanout)
        assert!(cone.leaves.contains(&ab.node));
    }

    #[test]
    fn test_refactor_with_params() {
        let pass = Refactor::with_params(5, 8);
        assert_eq!(pass.max_depth, 5);
        assert_eq!(pass.max_leaves, 8);
    }

    #[test]
    fn test_refactor_xor_pattern() {
        // XOR: a ^ b = (a & !b) | (!a & b)
        // Truth table: 0b0110
        let mut aig = Aig::new("test".to_string());
        let a = aig.add_input("a".to_string(), None);
        let b = aig.add_input("b".to_string(), None);

        // Build a ^ b the long way
        let a_and_notb = aig.add_and(
            AigLit::new(a),
            AigLit {
                node: b,
                inverted: true,
            },
        );
        let nota_and_b = aig.add_and(
            AigLit {
                node: a,
                inverted: true,
            },
            AigLit::new(b),
        );
        // OR = NAND of complements
        let nand = aig.add_and(
            AigLit {
                node: a_and_notb.node,
                inverted: true,
            },
            AigLit {
                node: nota_and_b.node,
                inverted: true,
            },
        );
        let xor_result = AigLit {
            node: nand.node,
            inverted: true,
        };

        aig.add_output("y".to_string(), xor_result);

        let before = aig.compute_stats().and_count;

        let mut pass = Refactor::new();
        let result = pass.run(&mut aig);

        // XOR should be optimally 3 gates, we built 3, so no improvement expected
        // But the pass should run without error
        assert!(result.ands_after <= before);
    }
}
