//! LUT Input Permutation Optimization
//!
//! When routing fails to reach a specific LUT input pin, this module
//! tries alternate input pins and permutes the truth table accordingly.
//!
//! A LUT4's truth table is indexed by: I3<<3 | I2<<2 | I1<<1 | I0
//! Swapping inputs requires permuting the truth table bits accordingly.

use skalp_lir::gate_netlist::{CellId, GateNetId, GateNetlist};
use std::collections::HashMap;

/// LUT input permutation result
#[derive(Debug, Clone)]
pub struct LutPermutation {
    /// The cell ID
    pub cell_id: CellId,
    /// Original input to new input mapping
    /// permutation[orig_idx] = new_idx means original input orig_idx
    /// should be connected to physical pin new_idx
    pub permutation: [usize; 4],
    /// Permuted truth table
    pub permuted_init: u64,
}

impl LutPermutation {
    /// Create an identity permutation (no change)
    pub fn identity(cell_id: CellId, init: u64) -> Self {
        Self {
            cell_id,
            permutation: [0, 1, 2, 3],
            permuted_init: init,
        }
    }

    /// Check if this is the identity permutation
    pub fn is_identity(&self) -> bool {
        self.permutation == [0, 1, 2, 3]
    }
}

/// Permute a LUT4 truth table based on input swaps
///
/// For a LUT4 with inputs I0, I1, I2, I3:
/// - Original index: i = I3<<3 | I2<<2 | I1<<1 | I0
/// - After permutation p, new_index j maps to old_index i where:
///   bit k of j corresponds to bit p[k] of i
pub fn permute_lut4_init(init: u16, permutation: &[usize; 4]) -> u16 {
    let mut result: u16 = 0;

    // For each output bit position in the new table
    for new_idx in 0..16u16 {
        // Map new index bits to old index bits
        let mut old_idx: u16 = 0;
        for (bit_pos, &orig_pos) in permutation.iter().enumerate() {
            // If bit bit_pos is set in new_idx, set bit orig_pos in old_idx
            if (new_idx >> bit_pos) & 1 == 1 {
                old_idx |= 1 << orig_pos;
            }
        }

        // Copy the bit from old position to new position
        if (init >> old_idx) & 1 == 1 {
            result |= 1 << new_idx;
        }
    }

    result
}

/// Permute a LUT6 truth table based on input swaps
#[allow(dead_code)]
pub fn permute_lut6_init(init: u64, permutation: &[usize; 6]) -> u64 {
    let mut result: u64 = 0;

    for new_idx in 0..64u64 {
        let mut old_idx: u64 = 0;
        for (bit_pos, &orig_pos) in permutation.iter().enumerate() {
            if (new_idx >> bit_pos) & 1 == 1 {
                old_idx |= 1 << orig_pos;
            }
        }

        if (init >> old_idx) & 1 == 1 {
            result |= 1 << new_idx;
        }
    }

    result
}

/// Swap two inputs in a permutation array
#[allow(dead_code)]
pub fn swap_inputs(permutation: &mut [usize; 4], a: usize, b: usize) {
    permutation.swap(a, b);
}

/// Generate all permutations of 4 inputs (24 total)
pub fn all_lut4_permutations() -> Vec<[usize; 4]> {
    let mut result = Vec::with_capacity(24);

    // Use Heap's algorithm to generate all permutations
    let mut perm = [0, 1, 2, 3];
    generate_permutations(&mut perm, 4, &mut result);

    result
}

fn generate_permutations(arr: &mut [usize; 4], n: usize, result: &mut Vec<[usize; 4]>) {
    if n == 1 {
        result.push(*arr);
        return;
    }

    for i in 0..n {
        generate_permutations(arr, n - 1, result);
        #[allow(unknown_lints, clippy::manual_is_multiple_of)]
        if n % 2 == 0 {
            arr.swap(i, n - 1);
        } else {
            arr.swap(0, n - 1);
        }
    }
}

/// LUT input permutation optimizer
pub struct LutPermutationOptimizer {
    /// Applied permutations for each LUT cell
    permutations: HashMap<CellId, LutPermutation>,
}

impl Default for LutPermutationOptimizer {
    fn default() -> Self {
        Self::new()
    }
}

impl LutPermutationOptimizer {
    /// Create a new optimizer
    pub fn new() -> Self {
        Self {
            permutations: HashMap::new(),
        }
    }

    /// Get the permutation for a cell (if any)
    pub fn get_permutation(&self, cell_id: CellId) -> Option<&LutPermutation> {
        self.permutations.get(&cell_id)
    }

    /// Get the permuted input index for a cell
    /// Returns the physical pin index that should be used for logical input `input_idx`
    pub fn get_permuted_input(&self, cell_id: CellId, input_idx: usize) -> usize {
        if let Some(perm) = self.permutations.get(&cell_id) {
            perm.permutation[input_idx]
        } else {
            input_idx
        }
    }

    /// Get the permuted LUT init value for a cell
    pub fn get_permuted_init(&self, cell_id: CellId, original_init: u64) -> u64 {
        if let Some(perm) = self.permutations.get(&cell_id) {
            perm.permuted_init
        } else {
            original_init
        }
    }

    /// Try to find a permutation that allows routing to succeed
    /// Returns the best permutation index (in the list of all permutations) and the permutation itself
    pub fn find_best_permutation<F>(
        &mut self,
        cell_id: CellId,
        original_init: u64,
        can_route: F,
    ) -> Option<LutPermutation>
    where
        F: Fn(&[usize; 4]) -> bool,
    {
        let all_perms = all_lut4_permutations();

        // Try identity first
        let identity = [0, 1, 2, 3];
        if can_route(&identity) {
            let perm = LutPermutation {
                cell_id,
                permutation: identity,
                permuted_init: original_init,
            };
            self.permutations.insert(cell_id, perm.clone());
            return Some(perm);
        }

        // Try all other permutations
        for permutation in all_perms {
            if can_route(&permutation) {
                let permuted_init = permute_lut4_init(original_init as u16, &permutation) as u64;
                let perm = LutPermutation {
                    cell_id,
                    permutation,
                    permuted_init,
                };
                self.permutations.insert(cell_id, perm.clone());
                return Some(perm);
            }
        }

        None
    }

    /// Apply a specific permutation to a cell
    pub fn apply_permutation(
        &mut self,
        cell_id: CellId,
        permutation: [usize; 4],
        original_init: u64,
    ) {
        let permuted_init = permute_lut4_init(original_init as u16, &permutation) as u64;
        self.permutations.insert(
            cell_id,
            LutPermutation {
                cell_id,
                permutation,
                permuted_init,
            },
        );
    }

    /// Get all applied permutations
    pub fn permutations(&self) -> &HashMap<CellId, LutPermutation> {
        &self.permutations
    }

    /// Apply permutations to a netlist by reordering cell inputs
    /// This modifies the netlist's input orderings based on stored permutations
    pub fn apply_to_netlist(&self, netlist: &mut GateNetlist) {
        for cell in &mut netlist.cells {
            if let Some(perm) = self.permutations.get(&cell.id) {
                if !perm.is_identity() && cell.inputs.len() == 4 {
                    // Reorder inputs according to permutation
                    let old_inputs = cell.inputs.clone();
                    for (new_idx, &orig_idx) in perm.permutation.iter().enumerate() {
                        if orig_idx < old_inputs.len() && new_idx < cell.inputs.len() {
                            cell.inputs[new_idx] = old_inputs[orig_idx];
                        }
                    }

                    // Update LUT init value
                    cell.lut_init = Some(perm.permuted_init);
                }
            }
        }
    }
}

/// Check if two input indices are swappable for routing optimization
/// Some input pairs may be more beneficial to swap than others
#[allow(dead_code)]
pub fn prioritized_swaps() -> Vec<(usize, usize)> {
    // Order swaps by likelihood of improving routability
    // In iCE40, input 3 often has different routing characteristics
    vec![
        (0, 1), // Swap adjacent inputs first
        (1, 2),
        (2, 3),
        (0, 2), // Then try wider swaps
        (1, 3),
        (0, 3),
    ]
}

/// Get the input permutation needed to use `available_pins` for `required_inputs`
/// Returns None if no valid permutation exists
#[allow(dead_code)]
pub fn find_permutation_for_pins(
    required_inputs: &[GateNetId],
    available_pins: &[bool; 4],
) -> Option<[usize; 4]> {
    // Count required vs available
    let required_count = required_inputs.len().min(4);
    let available_count = available_pins.iter().filter(|&&x| x).count();

    if available_count < required_count {
        return None;
    }

    // Find available pin indices
    let available_indices: Vec<usize> = available_pins
        .iter()
        .enumerate()
        .filter_map(|(i, &avail)| if avail { Some(i) } else { None })
        .collect();

    // Create permutation mapping required inputs to available pins
    let mut permutation = [0, 1, 2, 3];
    for (req_idx, &avail_idx) in available_indices.iter().enumerate().take(required_count) {
        if req_idx < 4 {
            permutation[req_idx] = avail_idx;
        }
    }

    Some(permutation)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identity_permutation() {
        // Identity permutation should not change the truth table
        let init: u16 = 0xF0F0; // Example truth table
        let perm = [0, 1, 2, 3];
        let result = permute_lut4_init(init, &perm);
        assert_eq!(result, init);
    }

    #[test]
    fn test_swap_01_permutation() {
        // Swap inputs 0 and 1
        // Original: T[i3,i2,i1,i0]
        // New: T'[i3,i2,i0,i1] (swap bits 0 and 1 of index)
        let init: u16 = 0b1010_0000_0000_0000; // Only T[13], T[15] are 1
        let perm = [1, 0, 2, 3]; // Swap inputs 0 and 1

        let result = permute_lut4_init(init, &perm);

        // After swap:
        // T[13] = T[1101] -> T'[1110] = T'[14]
        // T[15] = T[1111] -> T'[1111] = T'[15]
        assert_eq!(result, 0b1100_0000_0000_0000);
    }

    #[test]
    fn test_all_permutations_count() {
        let perms = all_lut4_permutations();
        assert_eq!(perms.len(), 24); // 4! = 24
    }

    #[test]
    fn test_all_permutations_unique() {
        let perms = all_lut4_permutations();
        let mut unique: std::collections::HashSet<[usize; 4]> = std::collections::HashSet::new();
        for p in perms {
            unique.insert(p);
        }
        assert_eq!(unique.len(), 24);
    }

    #[test]
    fn test_and_gate_permutation() {
        // 2-input AND gate: output = I0 & I1
        // Truth table: T[0]=0, T[1]=0, T[2]=0, T[3]=1
        // (only when both I0 and I1 are 1)
        let and_init: u16 = 0x8888; // AND repeated for each I3,I2 combo

        // Swap I0 and I2: should still be AND of the two inputs
        let perm = [2, 1, 0, 3];
        let result = permute_lut4_init(and_init, &perm);

        // Now it's AND of I2 and I1 instead of I0 and I1
        // Verify the function is still correct (just on different inputs)
        assert_ne!(result, 0); // Should have some 1s
        assert_ne!(result, 0xFFFF); // Should have some 0s
    }

    #[test]
    fn test_optimizer_identity() {
        let mut optimizer = LutPermutationOptimizer::new();
        let cell_id = CellId(0);
        let init: u64 = 0x1234;

        // When routing always succeeds, should use identity
        let result = optimizer.find_best_permutation(cell_id, init, |perm| perm == &[0, 1, 2, 3]);

        assert!(result.is_some());
        let perm = result.unwrap();
        assert!(perm.is_identity());
        assert_eq!(perm.permuted_init, init);
    }

    #[test]
    fn test_optimizer_find_alternate() {
        let mut optimizer = LutPermutationOptimizer::new();
        let cell_id = CellId(0);
        let init: u64 = 0x8000; // Simple truth table

        // Only allow routing when input 0 is mapped to physical pin 2
        let result = optimizer.find_best_permutation(cell_id, init, |perm| perm[0] == 2);

        assert!(result.is_some());
        let perm = result.unwrap();
        assert_eq!(perm.permutation[0], 2);
    }
}
