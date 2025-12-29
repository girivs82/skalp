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
        PrimitiveType::DffP
        | PrimitiveType::DffN
        | PrimitiveType::DffNeg
        | PrimitiveType::DffAR
        | PrimitiveType::DffAS => {
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

        // === Power Infrastructure ===
        PrimitiveType::LevelShifter { .. } => {
            // Level shifter passes signal through with voltage translation
            // Logic behavior is identical to buffer
            let a = inputs.first().copied().unwrap_or(false);
            vec![a]
        }

        PrimitiveType::IsolationCell {
            clamp_value,
            enable_active_high,
        } => {
            // inputs: [data_in, iso_en]
            let data = inputs.first().copied().unwrap_or(false);
            let iso_en = inputs.get(1).copied().unwrap_or(false);
            let is_isolated = if *enable_active_high { iso_en } else { !iso_en };

            if is_isolated {
                // Clamp to specified value (0, 1, or 2=hold)
                match clamp_value {
                    0 => vec![false],
                    1 => vec![true],
                    _ => vec![data], // Hold last value (simplified - pass through)
                }
            } else {
                vec![data]
            }
        }

        PrimitiveType::RetentionDff { .. } => {
            // inputs: [clk, d, save, restore] or [clk, d, rst, save, restore]
            // Same as DFF for logic simulation - retention behavior is state management
            let d = inputs.get(1).copied().unwrap_or(false);
            vec![d]
        }

        PrimitiveType::PowerSwitch { .. } => {
            // Power switches control power rails, no logic output
            vec![]
        }

        PrimitiveType::AlwaysOnBuf => {
            // Always-on buffer - same as regular buffer
            let a = inputs.first().copied().unwrap_or(false);
            vec![a]
        }

        // === Floating-Point Soft Macros ===
        // These are 32-bit operations that take 64 inputs (a[0..31], b[0..31])
        // and produce 32 outputs (result[0..31])
        PrimitiveType::Fp32Add => evaluate_fp32_operation(inputs, |a, b| a + b),

        PrimitiveType::Fp32Sub => evaluate_fp32_operation(inputs, |a, b| a - b),

        PrimitiveType::Fp32Mul => evaluate_fp32_operation(inputs, |a, b| a * b),

        PrimitiveType::Fp32Div => {
            evaluate_fp32_operation(inputs, |a, b| if b != 0.0 { a / b } else { f32::NAN })
        }

        // === NCL Threshold Gates ===
        // Note: These are simplified stateless evaluations.
        // Full NCL simulation with hysteresis (state-holding) is implemented in ncl_sim.rs (Phase 6).
        // THmn: Output HIGH when ≥m of n inputs are HIGH, LOW when all inputs are LOW
        PrimitiveType::Th12 => {
            // 1-of-2: OR with hysteresis (set when ≥1 input high, reset when all low)
            // Stateless version: just OR
            let count = inputs.iter().take(2).filter(|&&x| x).count();
            vec![count >= 1]
        }

        PrimitiveType::Th22 => {
            // 2-of-2: C-element (set when all inputs high, reset when all low)
            // Stateless version: AND
            let count = inputs.iter().take(2).filter(|&&x| x).count();
            vec![count >= 2]
        }

        PrimitiveType::Th13 => {
            let count = inputs.iter().take(3).filter(|&&x| x).count();
            vec![count >= 1]
        }

        PrimitiveType::Th23 => {
            let count = inputs.iter().take(3).filter(|&&x| x).count();
            vec![count >= 2]
        }

        PrimitiveType::Th33 => {
            let count = inputs.iter().take(3).filter(|&&x| x).count();
            vec![count >= 3]
        }

        PrimitiveType::Th14 => {
            let count = inputs.iter().take(4).filter(|&&x| x).count();
            vec![count >= 1]
        }

        PrimitiveType::Th24 => {
            let count = inputs.iter().take(4).filter(|&&x| x).count();
            vec![count >= 2]
        }

        PrimitiveType::Th34 => {
            let count = inputs.iter().take(4).filter(|&&x| x).count();
            vec![count >= 3]
        }

        PrimitiveType::Th44 => {
            let count = inputs.iter().take(4).filter(|&&x| x).count();
            vec![count >= 4]
        }

        PrimitiveType::Thmn { m, n } => {
            let count = inputs.iter().take(*n as usize).filter(|&&x| x).count();
            vec![count >= *m as usize]
        }

        PrimitiveType::ThmnW { m, n, weights } => {
            // Weighted threshold: sum(input[i] * weight[i]) >= m
            let weighted_sum: u32 = inputs
                .iter()
                .take(*n as usize)
                .zip(weights.iter())
                .map(|(&input, &weight)| if input { weight as u32 } else { 0 })
                .sum();
            vec![weighted_sum >= *m as u32]
        }

        PrimitiveType::NclCompletion { width } => {
            // Completion detection: all dual-rail signals must be in same phase
            // Input is width*2 bits (t0,f0, t1,f1, ...)
            // Complete when: all DATA (one rail high each) or all NULL (both rails low each)
            let w = *width as usize;
            let mut all_data = true;
            let mut all_null = true;

            for i in 0..w {
                let t = inputs.get(i * 2).copied().unwrap_or(false);
                let f = inputs.get(i * 2 + 1).copied().unwrap_or(false);

                // DATA: exactly one rail is high (t xor f)
                // NULL: both rails are low
                let is_data = t ^ f;
                let is_null = !t && !f;

                all_data = all_data && is_data;
                all_null = all_null && is_null;
            }

            vec![all_data || all_null]
        }
    }
}

/// Helper function to evaluate FP32 operations
/// Converts 64 boolean inputs (a[0..31], b[0..31]) to two f32 values,
/// applies the operation, and converts the result back to 32 boolean outputs
fn evaluate_fp32_operation<F>(inputs: &[bool], op: F) -> Vec<bool>
where
    F: Fn(f32, f32) -> f32,
{
    // Extract a[0..31] from inputs[0..31]
    let mut a_bits: u32 = 0;
    for i in 0..32 {
        if inputs.get(i).copied().unwrap_or(false) {
            a_bits |= 1 << i;
        }
    }

    // Extract b[0..31] from inputs[32..63]
    let mut b_bits: u32 = 0;
    for i in 0..32 {
        if inputs.get(32 + i).copied().unwrap_or(false) {
            b_bits |= 1 << i;
        }
    }

    // Convert to f32 and perform operation
    let a = f32::from_bits(a_bits);
    let b = f32::from_bits(b_bits);
    let result = op(a, b);

    // Convert result back to bits
    let result_bits = result.to_bits();

    // Return as 32 booleans
    (0..32).map(|i| (result_bits >> i) & 1 == 1).collect()
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
///
/// This function implements the digital-level effect of various fault types.
/// For timing violations and power-related faults, we model the observable
/// digital effect rather than the analog root cause.
fn apply_fault(outputs: &mut [bool], fault_type: &FaultType, inputs: &[bool]) {
    match fault_type {
        // ====================================================================
        // Permanent/Hard Faults
        // ====================================================================
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

        FaultType::Open { float_value } => {
            // Open fault - signal floats to technology-dependent value
            // In CMOS, typically floats high (weak pull-up from PMOS leakage)
            // but we allow configuration via float_value
            for out in outputs.iter_mut() {
                *out = *float_value;
            }
        }

        FaultType::Bridge {
            bridged_net: _,
            bridge_type,
        } => {
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

        // ====================================================================
        // Transient/Soft Faults
        // ====================================================================
        FaultType::BitFlip | FaultType::Transient => {
            // Invert all outputs
            for out in outputs.iter_mut() {
                *out = !*out;
            }
        }

        FaultType::MultiBitUpset { additional_bits: _ } => {
            // Multi-bit upset - flip the primary output
            // Additional bits are handled by the fault campaign (creates multiple faults)
            for out in outputs.iter_mut() {
                *out = !*out;
            }
        }

        // ====================================================================
        // Timing Violations
        // ====================================================================
        FaultType::TimingDelay { .. } => {
            // Timing delay - output the previous value (needs state tracking)
            // For combinational logic: output reflects stale input
            // Approximation: use first input as representative of "old" data
            for (i, out) in outputs.iter_mut().enumerate() {
                *out = inputs.get(i).copied().unwrap_or(false);
            }
        }

        FaultType::SetupViolation => {
            // Setup violation - FF captures PREVIOUS data value
            // For combinational gates, this manifests as output reflecting
            // the previous input state (approximated as first input)
            //
            // In actual FF evaluation (in gate_simulator.rs), this uses
            // the saved previous D value. Here we approximate for comb logic.
            for (i, out) in outputs.iter_mut().enumerate() {
                // Conservative: assume previous data was opposite
                *out = !inputs.get(i).copied().unwrap_or(false);
            }
        }

        FaultType::HoldViolation => {
            // Hold violation - FF captures CORRUPTED value
            // Model as: output is inverted (worst-case corruption)
            //
            // This is conservative - actual hold violation could result in
            // either value or metastability. Inversion ensures detection
            // by comparison mechanisms.
            for out in outputs.iter_mut() {
                *out = !*out;
            }
        }

        FaultType::Metastability {
            resolution_cycles: _,
        } => {
            // Metastability - output is undefined during resolution
            // Model as: random value (use simple hash-based pseudo-random)
            //
            // In actual implementation, the simulator tracks metastable state
            // and resolves after resolution_cycles. Here we just flip to
            // represent uncertainty.
            for out in outputs.iter_mut() {
                *out = !*out; // Flip as worst-case
            }
        }

        // ====================================================================
        // Power-Related Faults (Digital Effects)
        // ====================================================================
        FaultType::VoltageDropout => {
            // Voltage dropout manifests as setup violations (slower logic)
            // At the primitive level, treat as setup violation
            for (i, out) in outputs.iter_mut().enumerate() {
                *out = !inputs.get(i).copied().unwrap_or(false);
            }
        }

        FaultType::GroundBounce => {
            // Ground bounce manifests as transient glitch
            // Invert output for one cycle
            for out in outputs.iter_mut() {
                *out = !*out;
            }
        }

        FaultType::CrosstalkGlitch => {
            // Crosstalk glitch - brief inversion due to coupling
            for out in outputs.iter_mut() {
                *out = !*out;
            }
        }

        // ====================================================================
        // Clock Faults
        // ====================================================================
        FaultType::ClockGlitch => {
            // Clock glitch - extra clock edge
            // For combinational logic, this doesn't directly apply
            // The effect is handled in sequential element evaluation
            // Here we treat it as a transient on combinational outputs
            for out in outputs.iter_mut() {
                *out = !*out;
            }
        }

        FaultType::ClockStretch { stretch_cycles: _ } => {
            // Clock stretch - period temporarily longer
            // May cause setup violations on receiving FFs
            // For comb logic, treat as timing delay
            for (i, out) in outputs.iter_mut().enumerate() {
                *out = inputs.get(i).copied().unwrap_or(false);
            }
        }
    }
}

/// State for tracking timing violation effects on sequential elements
#[derive(Debug, Clone, Default)]
pub struct TimingFaultState {
    /// Previous D input value (for setup violation modeling)
    pub prev_d_value: bool,
    /// Metastability countdown (cycles until resolution)
    pub metastable_countdown: u32,
    /// Resolved value after metastability (set when countdown reaches 0)
    pub metastable_resolved_value: Option<bool>,
}

impl TimingFaultState {
    /// Create new timing fault state
    pub fn new() -> Self {
        Self::default()
    }

    /// Update previous D value (call each cycle before clock edge)
    pub fn update_prev_d(&mut self, d_value: bool) {
        self.prev_d_value = d_value;
    }

    /// Start metastability countdown
    pub fn enter_metastability(&mut self, resolution_cycles: u32) {
        self.metastable_countdown = resolution_cycles;
        self.metastable_resolved_value = None;
    }

    /// Tick metastability countdown, returns true if just resolved
    pub fn tick_metastability(&mut self) -> bool {
        if self.metastable_countdown > 0 {
            self.metastable_countdown -= 1;
            if self.metastable_countdown == 0 {
                // Resolve randomly (using simple deterministic "random" for reproducibility)
                // In practice, use a seeded RNG for the simulation
                self.metastable_resolved_value = Some(false); // Or use RNG
                return true;
            }
        }
        false
    }

    /// Check if currently in metastable state
    pub fn is_metastable(&self) -> bool {
        self.metastable_countdown > 0
    }

    /// Get resolved value (if resolved)
    pub fn get_resolved_value(&self) -> Option<bool> {
        self.metastable_resolved_value
    }
}

/// Evaluate a flip-flop with timing violation fault
///
/// This function handles the special cases for setup/hold violations
/// and metastability that require state tracking.
///
/// # Arguments
///
/// * `current_d` - Current D input value
/// * `clock_edge` - True if there's a positive clock edge this cycle
/// * `fault` - Optional fault injection configuration
/// * `state` - Mutable timing fault state for this FF
/// * `current_cycle` - Current simulation cycle
///
/// # Returns
///
/// The Q output value after considering faults
pub fn evaluate_dff_with_timing_fault(
    current_d: bool,
    clock_edge: bool,
    fault: Option<&FaultInjectionConfig>,
    state: &mut TimingFaultState,
    current_cycle: u64,
) -> bool {
    // Check if fault is active
    let active_fault = fault.filter(|f| f.is_active_at(current_cycle));

    match active_fault.map(|f| &f.fault_type) {
        Some(FaultType::SetupViolation) => {
            // Setup violation: capture previous D value instead of current
            if clock_edge {
                let result = state.prev_d_value;
                state.update_prev_d(current_d);
                result
            } else {
                state.update_prev_d(current_d);
                current_d // No edge, return current (will be latched)
            }
        }

        Some(FaultType::HoldViolation) => {
            // Hold violation: capture inverted value
            if clock_edge {
                state.update_prev_d(current_d);
                !current_d // Corrupted: inverted
            } else {
                state.update_prev_d(current_d);
                current_d
            }
        }

        Some(FaultType::Metastability { resolution_cycles }) => {
            if clock_edge && !state.is_metastable() {
                // Enter metastability on clock edge
                state.enter_metastability(*resolution_cycles);
            }

            // Tick countdown
            let just_resolved = state.tick_metastability();

            if state.is_metastable() {
                // Still metastable - return undefined (model as toggle)
                !current_d
            } else if just_resolved {
                // Just resolved - return resolved value
                state.get_resolved_value().unwrap_or(current_d)
            } else {
                // Normal operation
                state.update_prev_d(current_d);
                current_d
            }
        }

        _ => {
            // No timing fault or other fault type - normal operation
            state.update_prev_d(current_d);
            current_d
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
        assert_eq!(
            evaluate_primitive(&ptype, &[true, true, false]),
            vec![false]
        );
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
        assert_eq!(
            evaluate_primitive(&ptype, &[false, true, false]),
            vec![true]
        );
        // sel=1 -> d1
        assert_eq!(
            evaluate_primitive(&ptype, &[true, true, false]),
            vec![false]
        );
    }

    #[test]
    fn test_mux4() {
        let ptype = PrimitiveType::Mux4;
        // sel=00 -> d0
        assert_eq!(
            evaluate_primitive(&ptype, &[false, false, true, false, false, false]),
            vec![true]
        );
        // sel=01 -> d1
        assert_eq!(
            evaluate_primitive(&ptype, &[true, false, false, true, false, false]),
            vec![true]
        );
        // sel=10 -> d2
        assert_eq!(
            evaluate_primitive(&ptype, &[false, true, false, false, true, false]),
            vec![true]
        );
        // sel=11 -> d3
        assert_eq!(
            evaluate_primitive(&ptype, &[true, true, false, false, false, true]),
            vec![true]
        );
    }

    #[test]
    fn test_half_adder() {
        let ptype = PrimitiveType::HalfAdder;
        // 0+0 = 0, carry=0
        assert_eq!(
            evaluate_primitive(&ptype, &[false, false]),
            vec![false, false]
        );
        // 0+1 = 1, carry=0
        assert_eq!(
            evaluate_primitive(&ptype, &[false, true]),
            vec![true, false]
        );
        // 1+0 = 1, carry=0
        assert_eq!(
            evaluate_primitive(&ptype, &[true, false]),
            vec![true, false]
        );
        // 1+1 = 0, carry=1
        assert_eq!(evaluate_primitive(&ptype, &[true, true]), vec![false, true]);
    }

    #[test]
    fn test_full_adder() {
        let ptype = PrimitiveType::FullAdder;
        // 0+0+0 = 0, cout=0
        assert_eq!(
            evaluate_primitive(&ptype, &[false, false, false]),
            vec![false, false]
        );
        // 1+0+0 = 1, cout=0
        assert_eq!(
            evaluate_primitive(&ptype, &[true, false, false]),
            vec![true, false]
        );
        // 1+1+0 = 0, cout=1
        assert_eq!(
            evaluate_primitive(&ptype, &[true, true, false]),
            vec![false, true]
        );
        // 1+1+1 = 1, cout=1
        assert_eq!(
            evaluate_primitive(&ptype, &[true, true, true]),
            vec![true, true]
        );
    }

    #[test]
    fn test_tribuf_active_high() {
        let ptype = PrimitiveType::Tribuf {
            enable_active_high: true,
        };
        // enable=0 -> output=0 (tri-state approximation)
        assert_eq!(evaluate_primitive(&ptype, &[true, false]), vec![false]);
        // enable=1 -> output=data
        assert_eq!(evaluate_primitive(&ptype, &[true, true]), vec![true]);
        assert_eq!(evaluate_primitive(&ptype, &[false, true]), vec![false]);
    }

    #[test]
    fn test_constant() {
        assert_eq!(
            evaluate_primitive(&PrimitiveType::Constant { value: false }, &[]),
            vec![false]
        );
        assert_eq!(
            evaluate_primitive(&PrimitiveType::Constant { value: true }, &[]),
            vec![true]
        );
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
        assert_eq!(
            bits,
            vec![true, false, true, false, true, true, false, true]
        );
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

    // ========================================================================
    // Timing Violation Fault Tests
    // ========================================================================

    #[test]
    fn test_setup_violation_fault() {
        // Setup violation: output reflects previous input instead of current
        let ptype = PrimitiveType::Buf;
        let config = FaultInjectionConfig::setup_violation(PrimitiveId(0), 5, Some(3));

        // Before fault - normal operation
        let outputs = evaluate_primitive_with_fault(&ptype, &[true], Some(&config), 4);
        assert_eq!(outputs, vec![true]);

        // During fault - setup violation approximated as inverted input
        // (previous D value is modeled as opposite of current)
        let outputs = evaluate_primitive_with_fault(&ptype, &[true], Some(&config), 5);
        assert_eq!(outputs, vec![false]); // Inverted due to setup violation

        // Still during fault
        let outputs = evaluate_primitive_with_fault(&ptype, &[false], Some(&config), 6);
        assert_eq!(outputs, vec![true]); // Inverted

        // After fault duration ends - normal again
        let outputs = evaluate_primitive_with_fault(&ptype, &[true], Some(&config), 8);
        assert_eq!(outputs, vec![true]);
    }

    #[test]
    fn test_hold_violation_fault() {
        // Hold violation: output is inverted (corrupted)
        let ptype = PrimitiveType::Buf;
        let config = FaultInjectionConfig::hold_violation(PrimitiveId(0), 10, Some(2));

        // Before fault
        let outputs = evaluate_primitive_with_fault(&ptype, &[true], Some(&config), 9);
        assert_eq!(outputs, vec![true]);

        // During fault - hold violation inverts output
        let outputs = evaluate_primitive_with_fault(&ptype, &[true], Some(&config), 10);
        assert_eq!(outputs, vec![false]); // Inverted

        let outputs = evaluate_primitive_with_fault(&ptype, &[false], Some(&config), 11);
        assert_eq!(outputs, vec![true]); // Inverted

        // After fault
        let outputs = evaluate_primitive_with_fault(&ptype, &[true], Some(&config), 12);
        assert_eq!(outputs, vec![true]);
    }

    #[test]
    fn test_metastability_fault() {
        // Metastability: output is undefined (modeled as inverted)
        let ptype = PrimitiveType::Buf;
        let config = FaultInjectionConfig::metastability(PrimitiveId(0), 5, 3);

        // Before fault
        let outputs = evaluate_primitive_with_fault(&ptype, &[true], Some(&config), 4);
        assert_eq!(outputs, vec![true]);

        // During metastability - output is undefined (inverted in model)
        let outputs = evaluate_primitive_with_fault(&ptype, &[true], Some(&config), 5);
        assert_eq!(outputs, vec![false]); // Metastable: inverted

        let outputs = evaluate_primitive_with_fault(&ptype, &[false], Some(&config), 6);
        assert_eq!(outputs, vec![true]); // Still metastable: inverted

        // After resolution (duration = resolution_cycles + 1 = 4)
        let outputs = evaluate_primitive_with_fault(&ptype, &[true], Some(&config), 9);
        assert_eq!(outputs, vec![true]); // Normal
    }

    // ========================================================================
    // Power Effect Fault Tests
    // ========================================================================

    #[test]
    fn test_voltage_dropout_fault() {
        // Voltage dropout manifests as setup violation (slower logic)
        let ptype = PrimitiveType::And { inputs: 2 };
        let config = FaultInjectionConfig::voltage_dropout(PrimitiveId(0), 10, 5);

        // Before fault - normal
        let outputs = evaluate_primitive_with_fault(&ptype, &[true, true], Some(&config), 9);
        assert_eq!(outputs, vec![true]); // 1 AND 1 = 1

        // During fault - voltage dropout causes setup violation behavior
        // Output reflects previous input state (inverted first input)
        let outputs = evaluate_primitive_with_fault(&ptype, &[true, true], Some(&config), 10);
        assert_eq!(outputs, vec![false]); // Corrupted

        // Still during fault
        let outputs = evaluate_primitive_with_fault(&ptype, &[false, false], Some(&config), 12);
        assert_eq!(outputs, vec![true]); // Corrupted (inverted)

        // After fault ends
        let outputs = evaluate_primitive_with_fault(&ptype, &[true, true], Some(&config), 15);
        assert_eq!(outputs, vec![true]); // Normal
    }

    #[test]
    fn test_ground_bounce_fault() {
        // Ground bounce manifests as transient glitch (single cycle)
        let ptype = PrimitiveType::Or { inputs: 2 };
        let config = FaultInjectionConfig::ground_bounce(PrimitiveId(0), 20);

        // Before fault
        let outputs = evaluate_primitive_with_fault(&ptype, &[false, false], Some(&config), 19);
        assert_eq!(outputs, vec![false]); // 0 OR 0 = 0

        // During fault - ground bounce inverts output
        let outputs = evaluate_primitive_with_fault(&ptype, &[false, false], Some(&config), 20);
        assert_eq!(outputs, vec![true]); // Glitched to 1

        // After fault (single cycle duration)
        let outputs = evaluate_primitive_with_fault(&ptype, &[false, false], Some(&config), 21);
        assert_eq!(outputs, vec![false]); // Normal
    }

    #[test]
    fn test_crosstalk_glitch_fault() {
        // Crosstalk glitch - brief inversion due to coupling
        let ptype = PrimitiveType::Xor;
        let config = FaultInjectionConfig::crosstalk_glitch(PrimitiveId(0), 15);

        // Before fault
        let outputs = evaluate_primitive_with_fault(&ptype, &[true, false], Some(&config), 14);
        assert_eq!(outputs, vec![true]); // 1 XOR 0 = 1

        // During fault - crosstalk inverts
        let outputs = evaluate_primitive_with_fault(&ptype, &[true, false], Some(&config), 15);
        assert_eq!(outputs, vec![false]); // Glitched

        // After fault
        let outputs = evaluate_primitive_with_fault(&ptype, &[true, false], Some(&config), 16);
        assert_eq!(outputs, vec![true]); // Normal
    }

    // ========================================================================
    // Clock Fault Tests
    // ========================================================================

    #[test]
    fn test_clock_glitch_fault() {
        // Clock glitch - treated as transient on combinational outputs
        let ptype = PrimitiveType::Nand { inputs: 2 };
        let config = FaultInjectionConfig {
            target_primitive: PrimitiveId(0),
            fault_type: FaultType::ClockGlitch,
            inject_at_cycle: 25,
            duration: Some(1),
        };

        // Before fault
        let outputs = evaluate_primitive_with_fault(&ptype, &[true, true], Some(&config), 24);
        assert_eq!(outputs, vec![false]); // 1 NAND 1 = 0

        // During fault - clock glitch inverts output
        let outputs = evaluate_primitive_with_fault(&ptype, &[true, true], Some(&config), 25);
        assert_eq!(outputs, vec![true]); // Glitched

        // After fault
        let outputs = evaluate_primitive_with_fault(&ptype, &[true, true], Some(&config), 26);
        assert_eq!(outputs, vec![false]); // Normal
    }

    #[test]
    fn test_clock_stretch_fault() {
        // Clock stretch - period temporarily longer, may cause timing issues
        // For comb logic, output passes through (timing delay model)
        let ptype = PrimitiveType::And { inputs: 2 };
        let config = FaultInjectionConfig {
            target_primitive: PrimitiveId(0),
            fault_type: FaultType::ClockStretch { stretch_cycles: 2 },
            inject_at_cycle: 30,
            duration: Some(2),
        };

        // Before fault
        let outputs = evaluate_primitive_with_fault(&ptype, &[true, true], Some(&config), 29);
        assert_eq!(outputs, vec![true]); // 1 AND 1 = 1

        // During fault - clock stretch passes through first input
        let outputs = evaluate_primitive_with_fault(&ptype, &[true, false], Some(&config), 30);
        assert_eq!(outputs, vec![true]); // Passes first input

        // After fault
        let outputs = evaluate_primitive_with_fault(&ptype, &[true, false], Some(&config), 32);
        assert_eq!(outputs, vec![false]); // Normal: 1 AND 0 = 0
    }

    // ========================================================================
    // Open/Multi-bit Fault Tests
    // ========================================================================

    #[test]
    fn test_open_fault() {
        // Open fault - output floats to specified value (false = low)
        let ptype = PrimitiveType::Or { inputs: 2 };
        let config = FaultInjectionConfig {
            target_primitive: PrimitiveId(0),
            fault_type: FaultType::Open { float_value: false },
            inject_at_cycle: 0,
            duration: None, // Permanent
        };

        // With fault - output always 0 regardless of inputs
        let outputs = evaluate_primitive_with_fault(&ptype, &[true, true], Some(&config), 10);
        assert_eq!(outputs, vec![false]); // Open: floats to 0

        let outputs = evaluate_primitive_with_fault(&ptype, &[true, false], Some(&config), 20);
        assert_eq!(outputs, vec![false]); // Open: floats to 0
    }

    #[test]
    fn test_open_fault_float_high() {
        // Open fault with float_value = true (output floats high)
        let ptype = PrimitiveType::And { inputs: 2 };
        let config = FaultInjectionConfig {
            target_primitive: PrimitiveId(0),
            fault_type: FaultType::Open { float_value: true },
            inject_at_cycle: 0,
            duration: None,
        };

        // With fault - output always 1 regardless of inputs
        let outputs = evaluate_primitive_with_fault(&ptype, &[false, false], Some(&config), 10);
        assert_eq!(outputs, vec![true]); // Open: floats to 1
    }

    #[test]
    fn test_multi_bit_upset_fault() {
        // Multi-bit upset - multiple bits affected (treated similar to bit flip)
        let ptype = PrimitiveType::Buf;
        let config = FaultInjectionConfig {
            target_primitive: PrimitiveId(0),
            fault_type: FaultType::MultiBitUpset { additional_bits: 1 },
            inject_at_cycle: 5,
            duration: Some(1), // Transient
        };

        // Before fault
        let outputs = evaluate_primitive_with_fault(&ptype, &[true], Some(&config), 4);
        assert_eq!(outputs, vec![true]);

        // During fault - output flipped
        let outputs = evaluate_primitive_with_fault(&ptype, &[true], Some(&config), 5);
        assert_eq!(outputs, vec![false]); // MBU inverts

        // After fault
        let outputs = evaluate_primitive_with_fault(&ptype, &[true], Some(&config), 6);
        assert_eq!(outputs, vec![true]);
    }

    // ========================================================================
    // DFF-specific Timing Fault Tests
    // ========================================================================

    #[test]
    fn test_dff_setup_violation() {
        let mut state = TimingFaultState::new();
        let config = FaultInjectionConfig::setup_violation(PrimitiveId(0), 0, None);

        // Set up previous D value
        state.update_prev_d(false);

        // On clock edge with setup violation: should capture previous value (false)
        // not current value (true)
        let result = evaluate_dff_with_timing_fault(true, true, Some(&config), &mut state, 0);
        assert!(!result); // Captured previous D value
    }

    #[test]
    fn test_dff_hold_violation() {
        let mut state = TimingFaultState::new();
        let config = FaultInjectionConfig::hold_violation(PrimitiveId(0), 0, None);

        // On clock edge with hold violation: should capture inverted value
        let result = evaluate_dff_with_timing_fault(true, true, Some(&config), &mut state, 0);
        assert!(!result); // Inverted: true -> false
    }

    #[test]
    fn test_dff_metastability_enters_and_resolves() {
        let mut state = TimingFaultState::new();
        let config = FaultInjectionConfig::metastability(PrimitiveId(0), 0, 3);

        // Initially not metastable
        assert!(!state.is_metastable());

        // Clock edge triggers metastability, then immediately ticks (countdown: 3 -> 2)
        let result = evaluate_dff_with_timing_fault(true, true, Some(&config), &mut state, 0);
        assert!(!result); // Toggled: true -> false (metastable output)

        // Should now be metastable with countdown already decremented once
        assert!(state.is_metastable());
        assert_eq!(state.metastable_countdown, 2); // 3 -> 2 after first tick

        // Continue ticking (countdown: 2 -> 1)
        let result = evaluate_dff_with_timing_fault(true, false, Some(&config), &mut state, 1);
        assert!(!result); // Toggled: true -> false (still metastable)
        assert!(state.is_metastable());
        assert_eq!(state.metastable_countdown, 1);

        // Continue ticking (countdown: 1 -> 0, resolves)
        let _ = evaluate_dff_with_timing_fault(false, false, Some(&config), &mut state, 2);
        assert_eq!(state.metastable_countdown, 0);

        // Should have resolved
        assert!(!state.is_metastable());
        assert!(state.metastable_resolved_value.is_some());
    }

    #[test]
    fn test_timing_fault_state_update() {
        let mut state = TimingFaultState::new();

        // Initial state
        assert!(!state.prev_d_value);
        assert!(!state.is_metastable());
        assert!(state.get_resolved_value().is_none());

        // Update previous D
        state.update_prev_d(true);
        assert!(state.prev_d_value);

        // Enter metastability
        state.enter_metastability(5);
        assert!(state.is_metastable());
        assert_eq!(state.metastable_countdown, 5);

        // Tick without resolution
        let resolved = state.tick_metastability();
        assert!(!resolved);
        assert_eq!(state.metastable_countdown, 4);

        // Tick to resolution
        state.metastable_countdown = 1;
        let resolved = state.tick_metastability();
        assert!(resolved);
        assert!(!state.is_metastable());
        assert!(state.get_resolved_value().is_some());
    }
}
