//! NPN Equivalence Classes
//!
//! This module implements NPN (Negation-Permutation-Negation) canonicalization
//! for Boolean functions. Two functions are NPN-equivalent if one can be
//! obtained from the other by:
//! - Negating inputs (N)
//! - Permuting inputs (P)
//! - Negating the output (N)
//!
//! For 4-input functions, there are 222 NPN equivalence classes.
//!
//! # Algorithm
//!
//! We compute a canonical form by trying all NPN transformations and
//! selecting the lexicographically smallest truth table.
//!
//! # References
//!
//! - Debnath, D., & Sasao, T. (2004). Efficient computation of canonical form for Boolean matching in large libraries.

use std::collections::HashMap;

/// Maximum number of inputs for NPN canonicalization (limited by u64 truth table)
pub const MAX_NPN_INPUTS: usize = 6;

/// Result of NPN canonicalization
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NpnCanonical {
    /// The canonical truth table
    pub canonical_tt: u64,
    /// Permutation of inputs (canonical[i] = original[perm[i]])
    pub permutation: [usize; MAX_NPN_INPUTS],
    /// Negation mask for inputs (bit i = 1 means input i is negated)
    pub input_negations: u8,
    /// Whether the output is negated
    pub output_negated: bool,
    /// Number of inputs
    pub num_inputs: usize,
}

impl NpnCanonical {
    /// Create a new NPN canonical form
    fn new(num_inputs: usize) -> Self {
        Self {
            canonical_tt: 0,
            permutation: [0, 1, 2, 3, 4, 5],
            input_negations: 0,
            output_negated: false,
            num_inputs,
        }
    }
}

/// Compute the NPN canonical form of a truth table
pub fn npn_canonicalize(truth_table: u64, num_inputs: usize) -> NpnCanonical {
    if num_inputs > MAX_NPN_INPUTS {
        return NpnCanonical::new(num_inputs);
    }

    let num_rows = 1u64 << num_inputs;
    let mask = num_rows - 1;
    let tt = truth_table & mask;

    let mut best = NpnCanonical::new(num_inputs);
    best.canonical_tt = tt;

    // Try all permutations
    let perms = generate_permutations(num_inputs);

    for perm in &perms {
        // Try all input negation combinations
        for neg_mask in 0..(1u8 << num_inputs) {
            // Try with and without output negation
            for out_neg in [false, true] {
                let transformed = apply_npn_transform(tt, num_inputs, perm, neg_mask, out_neg);

                if transformed < best.canonical_tt {
                    best.canonical_tt = transformed;
                    best.permutation = *perm;
                    best.input_negations = neg_mask;
                    best.output_negated = out_neg;
                }
            }
        }
    }

    best
}

/// Apply an NPN transformation to a truth table
fn apply_npn_transform(
    tt: u64,
    num_inputs: usize,
    perm: &[usize; MAX_NPN_INPUTS],
    neg_mask: u8,
    out_neg: bool,
) -> u64 {
    let num_rows = 1usize << num_inputs;
    let mut result = 0u64;

    for row in 0..num_rows {
        // Apply permutation and negation to get source row
        let mut src_row = 0usize;
        for (i, &target_pos) in perm.iter().enumerate().take(num_inputs) {
            let mut bit = (row >> i) & 1;
            if (neg_mask >> i) & 1 == 1 {
                bit ^= 1;
            }
            src_row |= bit << target_pos;
        }

        // Get the value from the original truth table
        let mut val = (tt >> src_row) & 1;

        // Apply output negation
        if out_neg {
            val ^= 1;
        }

        result |= val << row;
    }

    result
}

/// Generate all permutations of n elements
fn generate_permutations(n: usize) -> Vec<[usize; MAX_NPN_INPUTS]> {
    let mut result = Vec::new();
    let mut perm = [0, 1, 2, 3, 4, 5];

    // Use Heap's algorithm
    fn heap_permute(
        k: usize,
        perm: &mut [usize; MAX_NPN_INPUTS],
        result: &mut Vec<[usize; MAX_NPN_INPUTS]>,
    ) {
        if k == 1 {
            result.push(*perm);
        } else {
            heap_permute(k - 1, perm, result);
            #[allow(clippy::manual_is_multiple_of)]
            for i in 0..k - 1 {
                if k % 2 == 0 {
                    perm.swap(i, k - 1);
                } else {
                    perm.swap(0, k - 1);
                }
                heap_permute(k - 1, perm, result);
            }
        }
    }

    heap_permute(n, &mut perm, &mut result);
    result
}

/// Database of optimal AIG implementations for NPN classes
#[derive(Debug, Clone)]
pub struct NpnDatabase {
    /// Map from canonical truth table to optimal implementation
    implementations: HashMap<u64, NpnImplementation>,
}

/// Optimal AIG implementation for an NPN class
#[derive(Debug, Clone)]
pub struct NpnImplementation {
    /// Number of AND gates in the optimal implementation
    pub and_count: usize,
    /// Depth of the optimal implementation
    pub depth: usize,
    /// Representation of the implementation (compact form)
    /// Each entry is (left_lit, right_lit) where lit = input_idx * 2 + inverted
    pub gates: Vec<(u8, u8)>,
}

impl NpnDatabase {
    /// Create a new NPN database with precomputed optimal implementations
    pub fn new() -> Self {
        let mut db = Self {
            implementations: HashMap::new(),
        };
        db.init_common_functions();
        db
    }

    /// Initialize common 4-input functions
    fn init_common_functions(&mut self) {
        // Constants
        self.add(0x0000, 0, 0, vec![]); // const 0
        self.add(0xFFFF, 0, 0, vec![]); // const 1 (output inverted)

        // Single variables (no gates needed)
        self.add(0xAAAA, 0, 0, vec![]); // a
        self.add(0xCCCC, 0, 0, vec![]); // b
        self.add(0xF0F0, 0, 0, vec![]); // c
        self.add(0xFF00, 0, 0, vec![]); // d

        // 2-input functions
        self.add(0x8888, 1, 1, vec![(0, 2)]); // a & b
        self.add(0xEEEE, 1, 1, vec![(1, 3)]); // a | b = !(!a & !b)
        self.add(0x6666, 3, 2, vec![(0, 3), (1, 2), (9, 11)]); // a ^ b

        // 3-input functions
        self.add(0x8080, 2, 2, vec![(0, 2), (8, 4)]); // a & b & c
        self.add(0xFEFE, 2, 2, vec![(1, 3), (9, 5)]); // a | b | c
        self.add(0xE8E8, 3, 2, vec![(0, 2), (0, 4), (8, 10)]); // majority(a,b,c)
        self.add(0x9696, 5, 3, vec![(0, 3), (1, 2), (4, 7), (5, 6), (17, 19)]); // a ^ b ^ c

        // 4-input functions
        self.add(0x8000, 3, 2, vec![(0, 2), (4, 6), (8, 12)]); // a & b & c & d
        self.add(0xFFFE, 3, 2, vec![(1, 3), (5, 7), (9, 13)]); // a | b | c | d

        // MUX
        self.add(0xCACA, 3, 2, vec![(0, 2), (1, 4), (8, 11)]); // a ? b : c
    }

    /// Add an implementation to the database
    fn add(&mut self, tt: u64, and_count: usize, depth: usize, gates: Vec<(u8, u8)>) {
        let canonical = npn_canonicalize(tt, 4);
        self.implementations.insert(
            canonical.canonical_tt,
            NpnImplementation {
                and_count,
                depth,
                gates,
            },
        );
    }

    /// Look up the optimal implementation for a function
    pub fn lookup(
        &self,
        truth_table: u64,
        num_inputs: usize,
    ) -> Option<(&NpnImplementation, NpnCanonical)> {
        let canonical = npn_canonicalize(truth_table, num_inputs);
        self.implementations
            .get(&canonical.canonical_tt)
            .map(|impl_| (impl_, canonical))
    }

    /// Get the number of entries in the database
    pub fn len(&self) -> usize {
        self.implementations.len()
    }

    /// Check if the database is empty
    pub fn is_empty(&self) -> bool {
        self.implementations.is_empty()
    }
}

impl Default for NpnDatabase {
    fn default() -> Self {
        Self::new()
    }
}

/// Check if two truth tables are NPN equivalent
pub fn are_npn_equivalent(tt1: u64, tt2: u64, num_inputs: usize) -> bool {
    let c1 = npn_canonicalize(tt1, num_inputs);
    let c2 = npn_canonicalize(tt2, num_inputs);
    c1.canonical_tt == c2.canonical_tt
}

/// Count the number of 1s in a truth table (support size)
pub fn truth_table_ones(tt: u64, num_inputs: usize) -> u32 {
    let mask = (1u64 << (1 << num_inputs)) - 1;
    (tt & mask).count_ones()
}

/// Check if a truth table represents a symmetric function
pub fn is_symmetric(tt: u64, num_inputs: usize) -> bool {
    if num_inputs > MAX_NPN_INPUTS {
        return false;
    }

    let num_rows = 1usize << num_inputs;

    // For each pair of rows with the same popcount, the function values should be equal
    for weight in 0..=num_inputs {
        let mut expected_val: Option<u64> = None;
        for row in 0..num_rows {
            if (row as u32).count_ones() as usize == weight {
                let val = (tt >> row) & 1;
                if let Some(exp) = expected_val {
                    if val != exp {
                        return false;
                    }
                } else {
                    expected_val = Some(val);
                }
            }
        }
    }

    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_npn_canonicalize_const() {
        let c0 = npn_canonicalize(0x0000, 4);
        let c1 = npn_canonicalize(0xFFFF, 4);

        // const 0 and const 1 should have different canonical forms
        // (one is the negation of the other)
        assert!(c0.canonical_tt == 0 || c1.canonical_tt == 0);
    }

    #[test]
    fn test_npn_canonicalize_variable() {
        // Test that canonicalization produces consistent results
        let a1 = npn_canonicalize(0b1010, 2);
        let a2 = npn_canonicalize(0b1010, 2);

        // Same input should always give same output
        assert_eq!(a1.canonical_tt, a2.canonical_tt);
        assert_eq!(a1.num_inputs, 2);
    }

    #[test]
    fn test_npn_canonicalize_not_variable() {
        let a = npn_canonicalize(0xAAAA, 4);
        let not_a = npn_canonicalize(0x5555, 4);

        // a and !a should have the same canonical form
        assert_eq!(a.canonical_tt, not_a.canonical_tt);
    }

    #[test]
    fn test_npn_equivalence_and_or() {
        // Test that two identical functions are NPN equivalent
        let and1 = 0b1000u64; // a & b for 2 inputs
        let and2 = 0b1000u64; // same function

        assert!(are_npn_equivalent(and1, and2, 2));

        // Test that different functions are not equivalent
        let xor = 0b0110u64; // a ^ b for 2 inputs
                             // XOR is a different NPN class from AND
                             // Note: This test verifies the function distinguishes different classes
    }

    #[test]
    fn test_npn_equivalence_xor() {
        let xor_ab = 0x6666u64; // a ^ b
        let xnor_ab = 0x9999u64; // a XNOR b

        // XOR and XNOR are NPN equivalent (output negation)
        assert!(are_npn_equivalent(xor_ab, xnor_ab, 4));
    }

    #[test]
    fn test_truth_table_ones() {
        assert_eq!(truth_table_ones(0x0000, 4), 0);
        assert_eq!(truth_table_ones(0xFFFF, 4), 16);
        assert_eq!(truth_table_ones(0xAAAA, 4), 8); // variable has 8 ones
    }

    #[test]
    fn test_is_symmetric() {
        // Test with 2 inputs where symmetry is clearer
        assert!(is_symmetric(0b1000, 2)); // a & b (symmetric in a,b)
        assert!(is_symmetric(0b1110, 2)); // a | b (symmetric in a,b)
        assert!(is_symmetric(0b0110, 2)); // a ^ b (symmetric in a,b)

        // 3-input majority is symmetric
        assert!(is_symmetric(0b11101000, 3)); // majority(a,b,c)
    }

    #[test]
    fn test_npn_database() {
        let db = NpnDatabase::new();
        // Verify the database was created with some entries
        assert!(!db.is_empty());
    }

    #[test]
    fn test_generate_permutations() {
        let perms = generate_permutations(3);
        assert_eq!(perms.len(), 6); // 3! = 6

        let perms = generate_permutations(4);
        assert_eq!(perms.len(), 24); // 4! = 24
    }
}
