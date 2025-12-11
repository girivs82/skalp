//! Gate-Level Primitive Evaluation
//!
//! Provides evaluation functions for technology-independent gate primitives
//! with optional fault injection support for ISO 26262 safety analysis.
//!
//! # Usage
//!
//! ```ignore
//! use skalp_sim::gate_eval::{evaluate_primitive, evaluate_primitive_with_fault};
//! use skalp_lir::lir::PrimitiveType;
//! use skalp_sim::sir::{FaultInjectionConfig, FaultType};
//!
//! // Simple evaluation without fault
//! let inputs = vec![true, true];
//! let outputs = evaluate_primitive(&PrimitiveType::And { inputs: 2 }, &inputs);
//! assert_eq!(outputs, vec![true]);
//!
//! // Evaluation with stuck-at-0 fault
//! let config = FaultInjectionConfig::stuck_at_0(PrimitiveId(0), 0);
//! let outputs = evaluate_primitive_with_fault(
//!     &PrimitiveType::And { inputs: 2 },
//!     &inputs,
//!     Some(&config),
//!     42, // current cycle
//! );
//! assert_eq!(outputs, vec![false]); // Fault forces output to 0
//! ```

use crate::sir::{FaultInjectionConfig, FaultType};
use skalp_lir::lir::PrimitiveType;

/// Evaluate a gate primitive without fault injection
///
/// # Arguments
///
/// * `ptype` - The primitive type to evaluate
/// * `inputs` - Input values as booleans (1-bit signals)
///
/// # Returns
///
/// Vector of output boolean values (typically 1, but 2 for adders/comparators)
pub fn evaluate_primitive(ptype: &PrimitiveType, inputs: &[bool]) -> Vec<bool> {
    match ptype {
        // === Combinational Logic ===
        PrimitiveType::And { inputs: n } => {
            let n = *n as usize;
            let result = inputs.iter().take(n).all(|&x| x);
            vec![result]
        }

        PrimitiveType::Or { inputs: n } => {
            let n = *n as usize;
            let result = inputs.iter().take(n).any(|&x| x);
            vec![result]
        }

        PrimitiveType::Nand { inputs: n } => {
            let n = *n as usize;
            let result = !inputs.iter().take(n).all(|&x| x);
            vec![result]
        }

        PrimitiveType::Nor { inputs: n } => {
            let n = *n as usize;
            let result = !inputs.iter().take(n).any(|&x| x);
            vec![result]
        }

        PrimitiveType::Xor => {
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            vec![a ^ b]
        }

        PrimitiveType::Xnor => {
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            vec![!(a ^ b)]
        }

        PrimitiveType::Inv => {
            let a = inputs.first().copied().unwrap_or(false);
            vec![!a]
        }

        PrimitiveType::Buf => {
            let a = inputs.first().copied().unwrap_or(false);
            vec![a]
        }

        PrimitiveType::ClkBuf => {
            // Clock buffer - same as regular buffer
            let a = inputs.first().copied().unwrap_or(false);
            vec![a]
        }

        PrimitiveType::Tribuf { enable_active_high } => {
            // inputs: [data, enable]
            let data = inputs.first().copied().unwrap_or(false);
            let enable = inputs.get(1).copied().unwrap_or(false);
            let is_enabled = if *enable_active_high { enable } else { !enable };
            // For simulation, output data when enabled, false otherwise (no Z state)
            vec![if is_enabled { data } else { false }]
        }

        PrimitiveType::Mux2 => {
            // inputs: [sel, d0, d1] - output: sel ? d1 : d0
            let sel = inputs.first().copied().unwrap_or(false);
            let d0 = inputs.get(1).copied().unwrap_or(false);
            let d1 = inputs.get(2).copied().unwrap_or(false);
            vec![if sel { d1 } else { d0 }]
        }

        PrimitiveType::Mux4 => {
            // inputs: [sel0, sel1, d0, d1, d2, d3]
            let sel0 = inputs.first().copied().unwrap_or(false);
            let sel1 = inputs.get(1).copied().unwrap_or(false);
            let sel = (sel1 as usize) * 2 + (sel0 as usize);
            let d = inputs.get(2 + sel).copied().unwrap_or(false);
            vec![d]
        }

        PrimitiveType::MuxN { select_bits } => {
            let n_sel = *select_bits as usize;
            let mut sel = 0usize;
            for i in 0..n_sel {
                if inputs.get(i).copied().unwrap_or(false) {
                    sel |= 1 << i;
                }
            }
            let d = inputs.get(n_sel + sel).copied().unwrap_or(false);
            vec![d]
        }

        PrimitiveType::Constant { value } => {
            vec![*value]
        }

        // === Arithmetic ===
        PrimitiveType::HalfAdder => {
            // inputs: [a, b], outputs: [sum, carry]
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            let sum = a ^ b;
            let carry = a & b;
            vec![sum, carry]
        }

        PrimitiveType::FullAdder => {
            // inputs: [a, b, cin], outputs: [sum, cout]
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            let cin = inputs.get(2).copied().unwrap_or(false);
            let sum = a ^ b ^ cin;
            let cout = (a & b) | (cin & (a ^ b));
            vec![sum, cout]
        }

        PrimitiveType::CompBit => {
            // inputs: [a, b, lt_in, eq_in], outputs: [lt_out, eq_out]
            let a = inputs.first().copied().unwrap_or(false);
            let b = inputs.get(1).copied().unwrap_or(false);
            let lt_in = inputs.get(2).copied().unwrap_or(false);
            let eq_in = inputs.get(3).copied().unwrap_or(true); // eq starts true

            // If previous bits weren't equal, propagate that decision
            if !eq_in {
                return vec![lt_in, false];
            }

            // This is the first differing bit (or still equal)
            let eq_out = a == b;
            let lt_out = if eq_out { lt_in } else { !a && b };
            vec![lt_out, eq_out]
        }

        // === Sequential Logic ===
        // For sequential elements, we return the current state (Q output)
        // The state update is handled separately by the simulation engine
        PrimitiveType::DffP | PrimitiveType::DffN | PrimitiveType::DffNeg | PrimitiveType::DffAR | PrimitiveType::DffAS => {
            // DFF inputs: [clk, d] - clock is at index 0, data at index 1
            // On clock edge, sample D input and store to Q output
            // Sequential behavior is handled by SequentialBlock processing
            let d = inputs.get(1).copied().unwrap_or(false);
            vec![d]
        }

        PrimitiveType::DffE => {
            // inputs: [clk, d, en, rst]
            let d = inputs.get(1).copied().unwrap_or(false);
            vec![d]
        }

        PrimitiveType::DffScan => {
            // inputs: [clk, d, scan_in, scan_en, rst]
            let d = inputs.get(1).copied().unwrap_or(false);
            let scan_in = inputs.get(2).copied().unwrap_or(false);
            let scan_en = inputs.get(3).copied().unwrap_or(false);
            vec![if scan_en { scan_in } else { d }]
        }

        PrimitiveType::Dlatch => {
            // inputs: [en, d]
            let d = inputs.get(1).copied().unwrap_or(false);
            vec![d]
        }

        PrimitiveType::SRlatch => {
            // inputs: [s, r]
            // For simulation, priority: reset > set
            let s = inputs.first().copied().unwrap_or(false);
            let r = inputs.get(1).copied().unwrap_or(false);
            if r {
                vec![false]
            } else if s {
                vec![true]
            } else {
                vec![false] // Hold state - needs external state tracking
            }
        }

        // === Memory ===
        PrimitiveType::MemCell | PrimitiveType::RegCell => {
            // inputs: [data_in, write_en, clk]
            // For combinational read, return data_in (actual behavior needs state)
            let data_in = inputs.first().copied().unwrap_or(false);
            vec![data_in]
        }
    }
}

/// Evaluate a gate primitive with optional fault injection
///
/// # Arguments
///
/// * `ptype` - The primitive type to evaluate
/// * `inputs` - Input values as booleans (1-bit signals)
/// * `fault` - Optional fault injection configuration
/// * `current_cycle` - Current simulation cycle (for checking if fault is active)
///
/// # Returns
///
/// Vector of output boolean values with fault effects applied if active
pub fn evaluate_primitive_with_fault(
    ptype: &PrimitiveType,
    inputs: &[bool],
    fault: Option<&FaultInjectionConfig>,
    current_cycle: u64,
) -> Vec<bool> {
    // First get the normal output
    let mut outputs = evaluate_primitive(ptype, inputs);

    // Apply fault if active
    if let Some(config) = fault {
        if config.is_active_at(current_cycle) {
            apply_fault(&mut outputs, &config.fault_type, inputs);
        }
    }

    outputs
}

/// Apply a fault effect to output values
fn apply_fault(outputs: &mut [bool], fault_type: &FaultType, inputs: &[bool]) {
    match fault_type {
        FaultType::StuckAt0 => {
            // All outputs forced to 0
            for out in outputs.iter_mut() {
                *out = false;
            }
        }

        FaultType::StuckAt1 => {
            // All outputs forced to 1
            for out in outputs.iter_mut() {
                *out = true;
            }
        }

        FaultType::BitFlip | FaultType::Transient => {
            // Invert all outputs
            for out in outputs.iter_mut() {
                *out = !*out;
            }
        }

        FaultType::TimingDelay { .. } => {
            // Timing delay - output the previous value (needs state tracking)
            // For now, just return the inputs as a rough approximation
            for (i, out) in outputs.iter_mut().enumerate() {
                *out = inputs.get(i).copied().unwrap_or(false);
            }
        }

        FaultType::Bridge { bridged_net: _, bridge_type } => {
            // Bridge fault - would need the bridged net's value
            // For now, apply a fixed effect based on bridge type
            match bridge_type {
                crate::sir::BridgeType::WiredAnd => {
                    // Assume bridged net is 1, so AND with output
                    // Output stays same (1 & out = out)
                }
                crate::sir::BridgeType::WiredOr => {
                    // Assume bridged net is 1, so OR with output = 1
                    for out in outputs.iter_mut() {
                        *out = true;
                    }
                }
                crate::sir::BridgeType::Dominant => {
                    // Assume bridged net dominates with 1
                    for out in outputs.iter_mut() {
                        *out = true;
                    }
                }
            }
        }
    }
}

/// Convert multi-bit value to boolean slice for primitive evaluation
///
/// # Arguments
///
/// * `value` - Multi-bit value as byte slice (little-endian)
/// * `width` - Number of bits
///
/// # Returns
///
/// Vector of individual bit values
pub fn value_to_bits(value: &[u8], width: usize) -> Vec<bool> {
    let mut bits = Vec::with_capacity(width);
    for i in 0..width {
        let byte_idx = i / 8;
        let bit_idx = i % 8;
        let bit = if byte_idx < value.len() {
            (value[byte_idx] >> bit_idx) & 1 != 0
        } else {
            false
        };
        bits.push(bit);
    }
    bits
}

/// Convert boolean slice back to multi-bit value
///
/// # Arguments
///
/// * `bits` - Individual bit values
///
/// # Returns
///
/// Multi-bit value as byte vector (little-endian)
pub fn bits_to_value(bits: &[bool]) -> Vec<u8> {
    let byte_count = bits.len().div_ceil(8);
    let mut value = vec![0u8; byte_count];
    for (i, &bit) in bits.iter().enumerate() {
        if bit {
            let byte_idx = i / 8;
            let bit_idx = i % 8;
            value[byte_idx] |= 1 << bit_idx;
        }
    }
    value
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use skalp_lir::lir::PrimitiveId;

    #[test]
    fn test_and_gate() {
        let ptype = PrimitiveType::And { inputs: 2 };
        assert_eq!(evaluate_primitive(&ptype, &[false, false]), vec![false]);
        assert_eq!(evaluate_primitive(&ptype, &[true, false]), vec![false]);
        assert_eq!(evaluate_primitive(&ptype, &[false, true]), vec![false]);
        assert_eq!(evaluate_primitive(&ptype, &[true, true]), vec![true]);
    }

    #[test]
    fn test_and_gate_3_inputs() {
        let ptype = PrimitiveType::And { inputs: 3 };
        assert_eq!(evaluate_primitive(&ptype, &[true, true, false]), vec![false]);
        assert_eq!(evaluate_primitive(&ptype, &[true, true, true]), vec![true]);
    }

    #[test]
    fn test_or_gate() {
        let ptype = PrimitiveType::Or { inputs: 2 };
        assert_eq!(evaluate_primitive(&ptype, &[false, false]), vec![false]);
        assert_eq!(evaluate_primitive(&ptype, &[true, false]), vec![true]);
        assert_eq!(evaluate_primitive(&ptype, &[false, true]), vec![true]);
        assert_eq!(evaluate_primitive(&ptype, &[true, true]), vec![true]);
    }

    #[test]
    fn test_nand_gate() {
        let ptype = PrimitiveType::Nand { inputs: 2 };
        assert_eq!(evaluate_primitive(&ptype, &[false, false]), vec![true]);
        assert_eq!(evaluate_primitive(&ptype, &[true, true]), vec![false]);
    }

    #[test]
    fn test_nor_gate() {
        let ptype = PrimitiveType::Nor { inputs: 2 };
        assert_eq!(evaluate_primitive(&ptype, &[false, false]), vec![true]);
        assert_eq!(evaluate_primitive(&ptype, &[true, false]), vec![false]);
    }

    #[test]
    fn test_xor_gate() {
        let ptype = PrimitiveType::Xor;
        assert_eq!(evaluate_primitive(&ptype, &[false, false]), vec![false]);
        assert_eq!(evaluate_primitive(&ptype, &[true, false]), vec![true]);
        assert_eq!(evaluate_primitive(&ptype, &[false, true]), vec![true]);
        assert_eq!(evaluate_primitive(&ptype, &[true, true]), vec![false]);
    }

    #[test]
    fn test_xnor_gate() {
        let ptype = PrimitiveType::Xnor;
        assert_eq!(evaluate_primitive(&ptype, &[false, false]), vec![true]);
        assert_eq!(evaluate_primitive(&ptype, &[true, true]), vec![true]);
        assert_eq!(evaluate_primitive(&ptype, &[true, false]), vec![false]);
    }

    #[test]
    fn test_inv_gate() {
        let ptype = PrimitiveType::Inv;
        assert_eq!(evaluate_primitive(&ptype, &[false]), vec![true]);
        assert_eq!(evaluate_primitive(&ptype, &[true]), vec![false]);
    }

    #[test]
    fn test_buf_gate() {
        let ptype = PrimitiveType::Buf;
        assert_eq!(evaluate_primitive(&ptype, &[false]), vec![false]);
        assert_eq!(evaluate_primitive(&ptype, &[true]), vec![true]);
    }

    #[test]
    fn test_mux2() {
        let ptype = PrimitiveType::Mux2;
        // sel=0 -> d0
        assert_eq!(evaluate_primitive(&ptype, &[false, true, false]), vec![true]);
        // sel=1 -> d1
        assert_eq!(evaluate_primitive(&ptype, &[true, true, false]), vec![false]);
    }

    #[test]
    fn test_mux4() {
        let ptype = PrimitiveType::Mux4;
        // sel=00 -> d0
        assert_eq!(evaluate_primitive(&ptype, &[false, false, true, false, false, false]), vec![true]);
        // sel=01 -> d1
        assert_eq!(evaluate_primitive(&ptype, &[true, false, false, true, false, false]), vec![true]);
        // sel=10 -> d2
        assert_eq!(evaluate_primitive(&ptype, &[false, true, false, false, true, false]), vec![true]);
        // sel=11 -> d3
        assert_eq!(evaluate_primitive(&ptype, &[true, true, false, false, false, true]), vec![true]);
    }

    #[test]
    fn test_half_adder() {
        let ptype = PrimitiveType::HalfAdder;
        // 0+0 = 0, carry=0
        assert_eq!(evaluate_primitive(&ptype, &[false, false]), vec![false, false]);
        // 0+1 = 1, carry=0
        assert_eq!(evaluate_primitive(&ptype, &[false, true]), vec![true, false]);
        // 1+0 = 1, carry=0
        assert_eq!(evaluate_primitive(&ptype, &[true, false]), vec![true, false]);
        // 1+1 = 0, carry=1
        assert_eq!(evaluate_primitive(&ptype, &[true, true]), vec![false, true]);
    }

    #[test]
    fn test_full_adder() {
        let ptype = PrimitiveType::FullAdder;
        // 0+0+0 = 0, cout=0
        assert_eq!(evaluate_primitive(&ptype, &[false, false, false]), vec![false, false]);
        // 1+0+0 = 1, cout=0
        assert_eq!(evaluate_primitive(&ptype, &[true, false, false]), vec![true, false]);
        // 1+1+0 = 0, cout=1
        assert_eq!(evaluate_primitive(&ptype, &[true, true, false]), vec![false, true]);
        // 1+1+1 = 1, cout=1
        assert_eq!(evaluate_primitive(&ptype, &[true, true, true]), vec![true, true]);
    }

    #[test]
    fn test_tribuf_active_high() {
        let ptype = PrimitiveType::Tribuf { enable_active_high: true };
        // enable=0 -> output=0 (tri-state approximation)
        assert_eq!(evaluate_primitive(&ptype, &[true, false]), vec![false]);
        // enable=1 -> output=data
        assert_eq!(evaluate_primitive(&ptype, &[true, true]), vec![true]);
        assert_eq!(evaluate_primitive(&ptype, &[false, true]), vec![false]);
    }

    #[test]
    fn test_constant() {
        assert_eq!(evaluate_primitive(&PrimitiveType::Constant { value: false }, &[]), vec![false]);
        assert_eq!(evaluate_primitive(&PrimitiveType::Constant { value: true }, &[]), vec![true]);
    }

    #[test]
    fn test_stuck_at_0_fault() {
        let ptype = PrimitiveType::And { inputs: 2 };
        let config = FaultInjectionConfig::stuck_at_0(PrimitiveId(0), 0);

        // Normal output would be true, but fault forces 0
        let outputs = evaluate_primitive_with_fault(&ptype, &[true, true], Some(&config), 10);
        assert_eq!(outputs, vec![false]);
    }

    #[test]
    fn test_stuck_at_1_fault() {
        let ptype = PrimitiveType::And { inputs: 2 };
        let config = FaultInjectionConfig::stuck_at_1(PrimitiveId(0), 0);

        // Normal output would be false, but fault forces 1
        let outputs = evaluate_primitive_with_fault(&ptype, &[false, false], Some(&config), 10);
        assert_eq!(outputs, vec![true]);
    }

    #[test]
    fn test_transient_fault() {
        let ptype = PrimitiveType::Buf;
        let config = FaultInjectionConfig::transient(PrimitiveId(0), 10);

        // Before fault - normal
        let outputs = evaluate_primitive_with_fault(&ptype, &[true], Some(&config), 9);
        assert_eq!(outputs, vec![true]);

        // During fault - inverted
        let outputs = evaluate_primitive_with_fault(&ptype, &[true], Some(&config), 10);
        assert_eq!(outputs, vec![false]);

        // After fault - normal again
        let outputs = evaluate_primitive_with_fault(&ptype, &[true], Some(&config), 11);
        assert_eq!(outputs, vec![true]);
    }

    #[test]
    fn test_bit_flip_fault() {
        let ptype = PrimitiveType::Or { inputs: 2 };
        let config = FaultInjectionConfig::bit_flip(PrimitiveId(0), 5, Some(3));

        // Normal: 0|0 = 0
        let outputs = evaluate_primitive_with_fault(&ptype, &[false, false], Some(&config), 4);
        assert_eq!(outputs, vec![false]);

        // With fault: 0|0 = 0, then flip = 1
        let outputs = evaluate_primitive_with_fault(&ptype, &[false, false], Some(&config), 6);
        assert_eq!(outputs, vec![true]);

        // After fault duration
        let outputs = evaluate_primitive_with_fault(&ptype, &[false, false], Some(&config), 8);
        assert_eq!(outputs, vec![false]);
    }

    #[test]
    fn test_no_fault() {
        let ptype = PrimitiveType::Xor;

        // With no fault config
        let outputs = evaluate_primitive_with_fault(&ptype, &[true, false], None, 100);
        assert_eq!(outputs, vec![true]);
    }

    #[test]
    fn test_value_to_bits() {
        let value = vec![0b10110101u8];
        let bits = value_to_bits(&value, 8);
        assert_eq!(bits, vec![true, false, true, false, true, true, false, true]);
    }

    #[test]
    fn test_bits_to_value() {
        let bits = vec![true, false, true, false, true, true, false, true];
        let value = bits_to_value(&bits);
        assert_eq!(value, vec![0b10110101u8]);
    }

    #[test]
    fn test_value_bits_roundtrip() {
        let original = vec![0xAB, 0xCD];
        let bits = value_to_bits(&original, 16);
        let result = bits_to_value(&bits);
        assert_eq!(result, original);
    }
}
