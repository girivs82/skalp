//! NCL (Null Convention Logic) Expansion Pass
//!
//! This module transforms synchronous boolean logic into NCL dual-rail logic.
//! It is the core transformation for asynchronous circuit synthesis.
//!
//! # NCL Basics
//!
//! NCL uses dual-rail encoding:
//! - Each logical bit is represented by two wires: (t, f) for true and false rails
//! - NULL state: t=0, f=0 (spacer between data)
//! - DATA_0: t=0, f=1 (logical 0)
//! - DATA_1: t=1, f=0 (logical 1)
//! - INVALID: t=1, f=1 (never occurs in valid NCL)
//!
//! # Threshold Gates (THmn)
//!
//! THmn gate: output = 1 when ≥m of n inputs are 1, output = 0 when all inputs are 0
//! Key gates:
//! - TH12: 1-of-2 (OR with hysteresis)
//! - TH22: 2-of-2 (C-element, AND with hysteresis)
//!
//! # NCL Gate Mappings
//!
//! | Boolean Op | True Rail           | False Rail          |
//! |------------|---------------------|---------------------|
//! | AND(a,b)   | TH22(a_t, b_t)      | TH12(a_f, b_f)      |
//! | OR(a,b)    | TH12(a_t, b_t)      | TH22(a_f, b_f)      |
//! | NOT(a)     | a_f                 | a_t                 |
//! | XOR(a,b)   | complex (see impl)  | complex (see impl)  |

use crate::lir::{Lir, LirNode, LirNodeId, LirOp, LirSignal, LirSignalId};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Configuration for NCL expansion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NclConfig {
    /// Use weak completion (only monitor outputs) vs strong (monitor all signals)
    pub use_weak_completion: bool,
    /// Maximum depth for completion detection tree (None = automatic)
    pub completion_tree_depth: Option<u32>,
    /// Insert NULL generator at inputs
    pub generate_null_wavefront: bool,
    /// Use opaque NCL arithmetic ops (NclAdd, NclSub) instead of bit-level expansion.
    /// This reduces LIR size by ~12x for arithmetic operations and lets tech_mapper
    /// handle the gate-level expansion directly.
    pub use_opaque_arithmetic: bool,
}

impl Default for NclConfig {
    fn default() -> Self {
        Self {
            use_weak_completion: true,
            completion_tree_depth: None,
            generate_null_wavefront: true,
            use_opaque_arithmetic: true, // Enable by default for performance
        }
    }
}

/// Result of NCL expansion
#[derive(Debug)]
pub struct NclExpandResult {
    /// The transformed NCL LIR
    pub lir: Lir,
    /// Mapping from original signals to their dual-rail pairs (t, f)
    pub dual_rail_map: HashMap<LirSignalId, DualRailPair>,
    /// Completion signals for each barrier stage
    pub stage_completions: Vec<LirSignalId>,
}

/// A signal representing dual-rail encoding.
///
/// For a W-bit logical signal, this is a 2W-bit signal where:
/// - Bits [0, 2, 4, ...] are the true rails
/// - Bits [1, 3, 5, ...] are the false rails
///
/// This allows the tech_mapper to correctly map NCL operations.
#[derive(Debug, Clone, Copy)]
pub struct DualRailSignal {
    /// The combined dual-rail signal ID
    pub id: LirSignalId,
    /// The original logical width (actual signal width is 2x this)
    pub logical_width: u32,
}

/// Legacy pair representation (for backward compatibility during transition)
#[derive(Debug, Clone, Copy)]
pub struct DualRailPair {
    /// True rail signal ID
    pub t: LirSignalId,
    /// False rail signal ID
    pub f: LirSignalId,
}

/// NCL expansion context
struct NclExpander {
    /// The LIR being built
    lir: Lir,
    /// Next signal ID
    next_signal_id: u32,
    /// Next node ID
    next_node_id: u32,
    /// Mapping from original signal to dual-rail pair
    dual_rail_map: HashMap<LirSignalId, DualRailPair>,
    /// Configuration
    config: NclConfig,
}

impl NclExpander {
    fn new(original_name: &str, config: NclConfig) -> Self {
        Self {
            lir: Lir::new(format!("{}_ncl", original_name)),
            next_signal_id: 0,
            next_node_id: 0,
            dual_rail_map: HashMap::new(),
            config,
        }
    }

    /// Allocate a new signal
    fn alloc_signal(&mut self, name: String, width: u32) -> LirSignalId {
        let id = LirSignalId(self.next_signal_id);
        self.next_signal_id += 1;
        self.lir.signals.push(LirSignal {
            id,
            name,
            width,
            driver: None,
            is_input: false,
            is_output: false,
            is_detection: false,
        });
        id
    }

    /// Allocate a new node
    fn alloc_node(
        &mut self,
        op: LirOp,
        inputs: Vec<LirSignalId>,
        output: LirSignalId,
    ) -> LirNodeId {
        self.alloc_node_with_path(op, inputs, output, String::new())
    }

    fn alloc_node_with_path(
        &mut self,
        op: LirOp,
        inputs: Vec<LirSignalId>,
        output: LirSignalId,
        path: String,
    ) -> LirNodeId {
        let id = LirNodeId(self.next_node_id);
        self.next_node_id += 1;
        self.lir.nodes.push(LirNode {
            id,
            op,
            inputs,
            output,
            path,
            clock: None,
            reset: None,
        });
        id
    }

    /// Create dual-rail signals for an original signal
    fn create_dual_rail(&mut self, original: &LirSignal) -> DualRailPair {
        let t = self.alloc_signal(format!("{}_t", original.name), original.width);
        let f = self.alloc_signal(format!("{}_f", original.name), original.width);
        DualRailPair { t, f }
    }

    /// Expand a single-rail AND to NCL dual-rail
    /// AND(a,b): t = TH22(a_t, b_t), f = TH12(a_f, b_f)
    ///
    /// Uses TH22 for true rail (C-element: both inputs must be high)
    /// Uses TH12 for false rail (OR: either input high)
    fn expand_and(&mut self, a: DualRailPair, b: DualRailPair, dest: DualRailPair, width: u32) {
        // NCL AND:
        // - True rail: TH22(a_t, b_t) - C-element, output 1 when both 1, output 0 when both 0
        // - False rail: TH12(a_f, b_f) - OR with hysteresis, output 1 when any 1, output 0 when both 0
        //
        // We use LirOp::Th22 and LirOp::Th12 which are proper NCL threshold gates
        // with hysteresis behavior - they hold their previous value during transitions.
        self.alloc_node(LirOp::Th22 { width }, vec![a.t, b.t], dest.t);
        self.alloc_node(LirOp::Th12 { width }, vec![a.f, b.f], dest.f);
    }

    /// Expand a single-rail OR to NCL dual-rail
    /// OR(a,b): t = TH12(a_t, b_t), f = TH22(a_f, b_f)
    ///
    /// Uses TH12 for true rail (OR: either input high)
    /// Uses TH22 for false rail (C-element: both inputs must be high)
    fn expand_or(&mut self, a: DualRailPair, b: DualRailPair, dest: DualRailPair, width: u32) {
        // NCL OR:
        // - True rail: TH12(a_t, b_t) - OR with hysteresis, output 1 when any 1
        // - False rail: TH22(a_f, b_f) - C-element, output 1 when both 1
        self.alloc_node(LirOp::Th12 { width }, vec![a.t, b.t], dest.t);
        self.alloc_node(LirOp::Th22 { width }, vec![a.f, b.f], dest.f);
    }

    /// Expand a single-rail NOT to NCL dual-rail
    /// NOT(a): t = a_f, f = a_t (just swap rails)
    /// Uses the destination dual-rail pair to ensure proper signal connectivity
    fn expand_not(&mut self, a: DualRailPair, dest: DualRailPair, width: u32) {
        // NCL NOT just swaps rails: dest.t gets a.f, dest.f gets a.t
        // Create two buffer operations to implement the swap
        // dest.t = buffer(a.f)
        self.alloc_node(LirOp::Buf { width }, vec![a.f], dest.t);
        // dest.f = buffer(a.t)
        self.alloc_node(LirOp::Buf { width }, vec![a.t], dest.f);
    }

    /// Expand a buffer (identity) to NCL dual-rail
    /// BUF(a): t = a_t, f = a_f (copy both rails)
    /// Uses the destination dual-rail pair to ensure proper signal connectivity
    fn expand_buf(&mut self, a: DualRailPair, dest: DualRailPair, width: u32) {
        // NCL buffer copies both rails: dest.t gets a.t, dest.f gets a.f
        // dest.t = buffer(a.t)
        self.alloc_node(LirOp::Buf { width }, vec![a.t], dest.t);
        // dest.f = buffer(a.f)
        self.alloc_node(LirOp::Buf { width }, vec![a.f], dest.f);
    }

    /// Expand XOR to NCL dual-rail
    /// XOR in dual-rail (using threshold gates for proper NCL behavior):
    /// - True rail: TH12(TH22(a_t, b_f), TH22(a_f, b_t))
    /// - False rail: TH12(TH22(a_t, b_t), TH22(a_f, b_f))
    fn expand_xor(&mut self, a: DualRailPair, b: DualRailPair, dest: DualRailPair, width: u32) {
        // Intermediate signals
        let at_bf = self.alloc_signal(format!("xor_at_bf_{}", self.next_signal_id), width);
        let af_bt = self.alloc_signal(format!("xor_af_bt_{}", self.next_signal_id), width);
        let at_bt = self.alloc_signal(format!("xor_at_bt_{}", self.next_signal_id), width);
        let af_bf = self.alloc_signal(format!("xor_af_bf_{}", self.next_signal_id), width);

        // TH22(a_t, b_f) - C-element for a_t AND b_f
        self.alloc_node(LirOp::Th22 { width }, vec![a.t, b.f], at_bf);
        // TH22(a_f, b_t) - C-element for a_f AND b_t
        self.alloc_node(LirOp::Th22 { width }, vec![a.f, b.t], af_bt);
        // True rail: TH12(at_bf, af_bt) - OR with hysteresis
        self.alloc_node(LirOp::Th12 { width }, vec![at_bf, af_bt], dest.t);

        // TH22(a_t, b_t) - C-element for a_t AND b_t
        self.alloc_node(LirOp::Th22 { width }, vec![a.t, b.t], at_bt);
        // TH22(a_f, b_f) - C-element for a_f AND b_f
        self.alloc_node(LirOp::Th22 { width }, vec![a.f, b.f], af_bf);
        // False rail: TH12(at_bt, af_bf) - OR with hysteresis
        self.alloc_node(LirOp::Th12 { width }, vec![at_bt, af_bf], dest.f);
    }

    /// Create completion detection for a set of dual-rail signals
    fn create_completion(&mut self, signals: &[DualRailPair]) -> LirSignalId {
        let completion = self.alloc_signal(format!("completion_{}", self.next_signal_id), 1);

        // Collect all t and f rails
        let mut inputs = Vec::new();
        for pair in signals {
            inputs.push(pair.t);
            inputs.push(pair.f);
        }

        let width = signals.len() as u32;
        self.alloc_node(LirOp::NclComplete { width }, inputs, completion);

        completion
    }

    /// Expand an NCL adder using proper ripple-carry chain
    /// NCL full adder: sum = a XOR b XOR cin, cout = majority(a,b,cin)
    /// Uses TH12/TH22 gates with bit-by-bit carry propagation
    fn expand_add(&mut self, a: DualRailPair, b: DualRailPair, dest: DualRailPair, width: u32) {
        // Build ripple-carry NCL adder bit-by-bit
        // For each bit: sum = XOR(XOR(a, b), cin), cout = MAJ(a, b, cin)

        let id = self.next_signal_id;
        self.next_signal_id += 1;

        // Collect sum bits for final concatenation
        let mut sum_t_bits: Vec<LirSignalId> = Vec::with_capacity(width as usize);
        let mut sum_f_bits: Vec<LirSignalId> = Vec::with_capacity(width as usize);

        // Initialize carry to 0 (dual-rail: t=0, f=1)
        let carry_init_t = self.alloc_signal(format!("add_cin_t_{}", id), 1);
        let carry_init_f = self.alloc_signal(format!("add_cin_f_{}", id), 1);
        let const_zero = self.alloc_signal(format!("add_zero_{}", id), 1);
        let const_one = self.alloc_signal(format!("add_one_{}", id), 1);
        self.alloc_node(LirOp::Constant { width: 1, value: 0 }, vec![], const_zero);
        self.alloc_node(LirOp::Constant { width: 1, value: 1 }, vec![], const_one);
        self.alloc_node(LirOp::Buf { width: 1 }, vec![const_zero], carry_init_t);
        self.alloc_node(LirOp::Buf { width: 1 }, vec![const_one], carry_init_f);

        let mut carry_t = carry_init_t;
        let mut carry_f = carry_init_f;

        for i in 0..width {
            // Extract bit i from a and b
            let a_t_i = self.alloc_signal(format!("add_a_t_{}_{}", id, i), 1);
            let a_f_i = self.alloc_signal(format!("add_a_f_{}_{}", id, i), 1);
            let b_t_i = self.alloc_signal(format!("add_b_t_{}_{}", id, i), 1);
            let b_f_i = self.alloc_signal(format!("add_b_f_{}_{}", id, i), 1);

            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: i,
                    low: i,
                },
                vec![a.t],
                a_t_i,
            );
            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: i,
                    low: i,
                },
                vec![a.f],
                a_f_i,
            );
            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: i,
                    low: i,
                },
                vec![b.t],
                b_t_i,
            );
            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: i,
                    low: i,
                },
                vec![b.f],
                b_f_i,
            );

            // XOR(a, b) for this bit
            let xor_ab_t = self.alloc_signal(format!("add_xor_ab_t_{}_{}", id, i), 1);
            let xor_ab_f = self.alloc_signal(format!("add_xor_ab_f_{}_{}", id, i), 1);

            // NCL XOR: t = TH12(TH22(a_t,b_f), TH22(a_f,b_t))
            //          f = TH12(TH22(a_t,b_t), TH22(a_f,b_f))
            let at_bf = self.alloc_signal(format!("add_at_bf_{}_{}", id, i), 1);
            let af_bt = self.alloc_signal(format!("add_af_bt_{}_{}", id, i), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a_t_i, b_f_i], at_bf);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a_f_i, b_t_i], af_bt);
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![at_bf, af_bt], xor_ab_t);

            let at_bt = self.alloc_signal(format!("add_at_bt_{}_{}", id, i), 1);
            let af_bf = self.alloc_signal(format!("add_af_bf_{}_{}", id, i), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a_t_i, b_t_i], at_bt);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a_f_i, b_f_i], af_bf);
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![at_bt, af_bf], xor_ab_f);

            // XOR(xor_ab, cin) for sum
            let sum_t_i = self.alloc_signal(format!("add_sum_t_{}_{}", id, i), 1);
            let sum_f_i = self.alloc_signal(format!("add_sum_f_{}_{}", id, i), 1);

            let xor_ct = self.alloc_signal(format!("add_xor_ct_{}_{}", id, i), 1);
            let xor_cf = self.alloc_signal(format!("add_xor_cf_{}_{}", id, i), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![xor_ab_t, carry_f], xor_ct);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![xor_ab_f, carry_t], xor_cf);
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![xor_ct, xor_cf], sum_t_i);

            let xor_dt = self.alloc_signal(format!("add_xor_dt_{}_{}", id, i), 1);
            let xor_df = self.alloc_signal(format!("add_xor_df_{}_{}", id, i), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![xor_ab_t, carry_t], xor_dt);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![xor_ab_f, carry_f], xor_df);
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![xor_dt, xor_df], sum_f_i);

            sum_t_bits.push(sum_t_i);
            sum_f_bits.push(sum_f_i);

            // Carry out = MAJ(a, b, cin) = OR(AND(a,b), AND(b,cin), AND(a,cin))
            // NCL AND: t = TH22(a_t, b_t), f = TH12(a_f, b_f)
            // NCL OR:  t = TH12(a_t, b_t), f = TH22(a_f, b_f)

            // AND(a, b)
            let and_ab_t = self.alloc_signal(format!("add_and_ab_t_{}_{}", id, i), 1);
            let and_ab_f = self.alloc_signal(format!("add_and_ab_f_{}_{}", id, i), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a_t_i, b_t_i], and_ab_t);
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![a_f_i, b_f_i], and_ab_f);

            // AND(b, cin)
            let and_bc_t = self.alloc_signal(format!("add_and_bc_t_{}_{}", id, i), 1);
            let and_bc_f = self.alloc_signal(format!("add_and_bc_f_{}_{}", id, i), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![b_t_i, carry_t], and_bc_t);
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![b_f_i, carry_f], and_bc_f);

            // AND(a, cin)
            let and_ac_t = self.alloc_signal(format!("add_and_ac_t_{}_{}", id, i), 1);
            let and_ac_f = self.alloc_signal(format!("add_and_ac_f_{}_{}", id, i), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a_t_i, carry_t], and_ac_t);
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![a_f_i, carry_f], and_ac_f);

            // OR of all three ANDs for carry true rail
            let or_ab_bc_t = self.alloc_signal(format!("add_or_ab_bc_t_{}_{}", id, i), 1);
            self.alloc_node(
                LirOp::Th12 { width: 1 },
                vec![and_ab_t, and_bc_t],
                or_ab_bc_t,
            );
            let new_carry_t = self.alloc_signal(format!("add_cout_t_{}_{}", id, i), 1);
            self.alloc_node(
                LirOp::Th12 { width: 1 },
                vec![or_ab_bc_t, and_ac_t],
                new_carry_t,
            );

            // AND of all three ORs for carry false rail (De Morgan: NOT(OR(...)) = AND(NOT(...)))
            let or_ab_bc_f = self.alloc_signal(format!("add_or_ab_bc_f_{}_{}", id, i), 1);
            self.alloc_node(
                LirOp::Th22 { width: 1 },
                vec![and_ab_f, and_bc_f],
                or_ab_bc_f,
            );
            let new_carry_f = self.alloc_signal(format!("add_cout_f_{}_{}", id, i), 1);
            self.alloc_node(
                LirOp::Th22 { width: 1 },
                vec![or_ab_bc_f, and_ac_f],
                new_carry_f,
            );

            carry_t = new_carry_t;
            carry_f = new_carry_f;
        }

        // Concatenate sum bits into final result
        // Note: Concat uses {a, b, ...} convention where first element is MSB
        // We need to reverse since sum_t_bits[0] is bit 0 (LSB)
        if width == 1 {
            self.alloc_node(LirOp::Buf { width: 1 }, vec![sum_t_bits[0]], dest.t);
            self.alloc_node(LirOp::Buf { width: 1 }, vec![sum_f_bits[0]], dest.f);
        } else {
            let widths: Vec<u32> = vec![1; width as usize];
            // Reverse to put MSB first in concat
            let sum_t_reversed: Vec<_> = sum_t_bits.into_iter().rev().collect();
            let sum_f_reversed: Vec<_> = sum_f_bits.into_iter().rev().collect();
            self.alloc_node(
                LirOp::Concat {
                    widths: widths.clone(),
                },
                sum_t_reversed,
                dest.t,
            );
            self.alloc_node(LirOp::Concat { widths }, sum_f_reversed, dest.f);
        }
    }

    /// Expand an NCL subtractor using proper two's complement: a - b = a + (~b) + 1
    /// Uses ripple-carry chain with carry-in = 1
    fn expand_sub(&mut self, a: DualRailPair, b: DualRailPair, dest: DualRailPair, width: u32) {
        // Subtraction: a - b = a + (~b) + 1
        // In NCL, inverting b means swapping t and f rails
        // We use the same ripple-carry but with carry-in = 1

        let id = self.next_signal_id;
        self.next_signal_id += 1;

        // Invert b by swapping rails (NCL NOT)
        let b_inv = DualRailPair { t: b.f, f: b.t };

        // Collect difference bits for final concatenation
        let mut diff_t_bits: Vec<LirSignalId> = Vec::with_capacity(width as usize);
        let mut diff_f_bits: Vec<LirSignalId> = Vec::with_capacity(width as usize);

        // Initialize carry to 1 (dual-rail: t=1, f=0) for two's complement
        let carry_init_t = self.alloc_signal(format!("sub_cin_t_{}", id), 1);
        let carry_init_f = self.alloc_signal(format!("sub_cin_f_{}", id), 1);
        let const_zero = self.alloc_signal(format!("sub_zero_{}", id), 1);
        let const_one = self.alloc_signal(format!("sub_one_{}", id), 1);
        self.alloc_node(LirOp::Constant { width: 1, value: 0 }, vec![], const_zero);
        self.alloc_node(LirOp::Constant { width: 1, value: 1 }, vec![], const_one);
        self.alloc_node(LirOp::Buf { width: 1 }, vec![const_one], carry_init_t);
        self.alloc_node(LirOp::Buf { width: 1 }, vec![const_zero], carry_init_f);

        let mut carry_t = carry_init_t;
        let mut carry_f = carry_init_f;

        for i in 0..width {
            // Extract bit i from a and inverted b
            let a_t_i = self.alloc_signal(format!("sub_a_t_{}_{}", id, i), 1);
            let a_f_i = self.alloc_signal(format!("sub_a_f_{}_{}", id, i), 1);
            let binv_t_i = self.alloc_signal(format!("sub_binv_t_{}_{}", id, i), 1);
            let binv_f_i = self.alloc_signal(format!("sub_binv_f_{}_{}", id, i), 1);

            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: i,
                    low: i,
                },
                vec![a.t],
                a_t_i,
            );
            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: i,
                    low: i,
                },
                vec![a.f],
                a_f_i,
            );
            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: i,
                    low: i,
                },
                vec![b_inv.t],
                binv_t_i,
            );
            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: i,
                    low: i,
                },
                vec![b_inv.f],
                binv_f_i,
            );

            // XOR(a, ~b) for this bit
            let xor_ab_t = self.alloc_signal(format!("sub_xor_ab_t_{}_{}", id, i), 1);
            let xor_ab_f = self.alloc_signal(format!("sub_xor_ab_f_{}_{}", id, i), 1);

            let at_bf = self.alloc_signal(format!("sub_at_bf_{}_{}", id, i), 1);
            let af_bt = self.alloc_signal(format!("sub_af_bt_{}_{}", id, i), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a_t_i, binv_f_i], at_bf);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a_f_i, binv_t_i], af_bt);
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![at_bf, af_bt], xor_ab_t);

            let at_bt = self.alloc_signal(format!("sub_at_bt_{}_{}", id, i), 1);
            let af_bf = self.alloc_signal(format!("sub_af_bf_{}_{}", id, i), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a_t_i, binv_t_i], at_bt);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a_f_i, binv_f_i], af_bf);
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![at_bt, af_bf], xor_ab_f);

            // XOR(xor_ab, cin) for difference
            let diff_t_i = self.alloc_signal(format!("sub_diff_t_{}_{}", id, i), 1);
            let diff_f_i = self.alloc_signal(format!("sub_diff_f_{}_{}", id, i), 1);

            let xor_ct = self.alloc_signal(format!("sub_xor_ct_{}_{}", id, i), 1);
            let xor_cf = self.alloc_signal(format!("sub_xor_cf_{}_{}", id, i), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![xor_ab_t, carry_f], xor_ct);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![xor_ab_f, carry_t], xor_cf);
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![xor_ct, xor_cf], diff_t_i);

            let xor_dt = self.alloc_signal(format!("sub_xor_dt_{}_{}", id, i), 1);
            let xor_df = self.alloc_signal(format!("sub_xor_df_{}_{}", id, i), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![xor_ab_t, carry_t], xor_dt);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![xor_ab_f, carry_f], xor_df);
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![xor_dt, xor_df], diff_f_i);

            diff_t_bits.push(diff_t_i);
            diff_f_bits.push(diff_f_i);

            // Carry out = MAJ(a, ~b, cin)
            let and_ab_t = self.alloc_signal(format!("sub_and_ab_t_{}_{}", id, i), 1);
            let and_ab_f = self.alloc_signal(format!("sub_and_ab_f_{}_{}", id, i), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a_t_i, binv_t_i], and_ab_t);
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![a_f_i, binv_f_i], and_ab_f);

            let and_bc_t = self.alloc_signal(format!("sub_and_bc_t_{}_{}", id, i), 1);
            let and_bc_f = self.alloc_signal(format!("sub_and_bc_f_{}_{}", id, i), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![binv_t_i, carry_t], and_bc_t);
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![binv_f_i, carry_f], and_bc_f);

            let and_ac_t = self.alloc_signal(format!("sub_and_ac_t_{}_{}", id, i), 1);
            let and_ac_f = self.alloc_signal(format!("sub_and_ac_f_{}_{}", id, i), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a_t_i, carry_t], and_ac_t);
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![a_f_i, carry_f], and_ac_f);

            let or_ab_bc_t = self.alloc_signal(format!("sub_or_ab_bc_t_{}_{}", id, i), 1);
            self.alloc_node(
                LirOp::Th12 { width: 1 },
                vec![and_ab_t, and_bc_t],
                or_ab_bc_t,
            );
            let new_carry_t = self.alloc_signal(format!("sub_cout_t_{}_{}", id, i), 1);
            self.alloc_node(
                LirOp::Th12 { width: 1 },
                vec![or_ab_bc_t, and_ac_t],
                new_carry_t,
            );

            let or_ab_bc_f = self.alloc_signal(format!("sub_or_ab_bc_f_{}_{}", id, i), 1);
            self.alloc_node(
                LirOp::Th22 { width: 1 },
                vec![and_ab_f, and_bc_f],
                or_ab_bc_f,
            );
            let new_carry_f = self.alloc_signal(format!("sub_cout_f_{}_{}", id, i), 1);
            self.alloc_node(
                LirOp::Th22 { width: 1 },
                vec![or_ab_bc_f, and_ac_f],
                new_carry_f,
            );

            carry_t = new_carry_t;
            carry_f = new_carry_f;
        }

        // Concatenate difference bits into final result
        // Note: Concat uses {a, b, ...} convention where first element is MSB
        // We need to reverse since diff_t_bits[0] is bit 0 (LSB)
        if width == 1 {
            self.alloc_node(LirOp::Buf { width: 1 }, vec![diff_t_bits[0]], dest.t);
            self.alloc_node(LirOp::Buf { width: 1 }, vec![diff_f_bits[0]], dest.f);
        } else {
            let widths: Vec<u32> = vec![1; width as usize];
            // Reverse to put MSB first in concat
            let diff_t_reversed: Vec<_> = diff_t_bits.into_iter().rev().collect();
            let diff_f_reversed: Vec<_> = diff_f_bits.into_iter().rev().collect();
            self.alloc_node(
                LirOp::Concat {
                    widths: widths.clone(),
                },
                diff_t_reversed,
                dest.t,
            );
            self.alloc_node(LirOp::Concat { widths }, diff_f_reversed, dest.f);
        }
    }

    /// Expand an NCL adder using opaque NclAdd operation (LIR optimization).
    ///
    /// This emits a single LirOp::NclAdd instead of bit-by-bit ripple-carry expansion.
    /// The tech_mapper handles the gate-level expansion directly.
    ///
    /// For a W-bit adder, this reduces LIR from ~25*W nodes to ~2*W+3 nodes (12x reduction).
    /// The output is interleaved format [t0, f0, t1, f1, ...] which we deinterleave.
    fn expand_add_opaque(
        &mut self,
        a: DualRailPair,
        b: DualRailPair,
        dest: DualRailPair,
        width: u32,
    ) {
        let id = self.next_signal_id;
        self.next_signal_id += 1;

        // Create interleaved output signal (2 * width bits)
        let interleaved = self.alloc_signal(format!("ncl_add_interleaved_{}", id), width * 2);

        // Emit NclAdd: inputs are a_t, a_f, b_t, b_f (each width bits)
        // Output is interleaved: [t0, f0, t1, f1, ...]
        // Use unique path to avoid cell name collisions when multiple ADD ops exist
        self.alloc_node_with_path(
            LirOp::NclAdd { width },
            vec![a.t, a.f, b.t, b.f],
            interleaved,
            format!("ncl_add_{}", id),
        );

        // Deinterleave: extract t and f rails from interleaved output
        self.deinterleave_to_pair(interleaved, dest, width);
    }

    /// Expand an NCL subtractor using opaque NclSub operation (LIR optimization).
    ///
    /// Similar to expand_add_opaque but for subtraction.
    /// Note: NclSub expects the inverted b rails (swapped), so we pass b.f as the "true" rail
    /// and b.t as the "false" rail to implement a - b = a + (~b) + 1.
    fn expand_sub_opaque(
        &mut self,
        a: DualRailPair,
        b: DualRailPair,
        dest: DualRailPair,
        width: u32,
    ) {
        let id = self.next_signal_id;
        self.next_signal_id += 1;

        // Create interleaved output signal (2 * width bits)
        let interleaved = self.alloc_signal(format!("ncl_sub_interleaved_{}", id), width * 2);

        // Emit NclSub: inputs are a_t, a_f, ~b_t (=b_f), ~b_f (=b_t)
        // The inversion of b (swap rails) implements two's complement: a + (~b) + 1
        // Output is interleaved: [t0, f0, t1, f1, ...]
        // Use unique path to avoid cell name collisions when multiple SUB ops exist
        self.alloc_node_with_path(
            LirOp::NclSub { width },
            vec![a.t, a.f, b.f, b.t], // Note: b rails are swapped for inversion
            interleaved,
            format!("ncl_sub_{}", id),
        );

        // Deinterleave: extract t and f rails from interleaved output
        self.deinterleave_to_pair(interleaved, dest, width);
    }

    /// Helper to deinterleave an interleaved signal [t0, f0, t1, f1, ...] into a DualRailPair.
    fn deinterleave_to_pair(&mut self, interleaved: LirSignalId, dest: DualRailPair, width: u32) {
        let id = self.next_signal_id;
        self.next_signal_id += 1;

        if width == 1 {
            // Special case: width 1, just use RangeSelect
            self.alloc_node(
                LirOp::RangeSelect {
                    width: 2,
                    high: 0,
                    low: 0,
                },
                vec![interleaved],
                dest.t,
            );
            self.alloc_node(
                LirOp::RangeSelect {
                    width: 2,
                    high: 1,
                    low: 1,
                },
                vec![interleaved],
                dest.f,
            );
        } else {
            // Extract individual bits and concatenate
            let mut t_bits: Vec<LirSignalId> = Vec::with_capacity(width as usize);
            let mut f_bits: Vec<LirSignalId> = Vec::with_capacity(width as usize);

            for i in 0..width {
                let t_bit = self.alloc_signal(format!("deint_t{}_{}", i, id), 1);
                let f_bit = self.alloc_signal(format!("deint_f{}_{}", i, id), 1);

                // t_bit[i] = interleaved[2*i]
                self.alloc_node(
                    LirOp::RangeSelect {
                        width: width * 2,
                        high: 2 * i,
                        low: 2 * i,
                    },
                    vec![interleaved],
                    t_bit,
                );
                // f_bit[i] = interleaved[2*i + 1]
                self.alloc_node(
                    LirOp::RangeSelect {
                        width: width * 2,
                        high: 2 * i + 1,
                        low: 2 * i + 1,
                    },
                    vec![interleaved],
                    f_bit,
                );

                t_bits.push(t_bit);
                f_bits.push(f_bit);
            }

            // Concatenate bits into dest.t and dest.f
            // Reverse because Concat puts first element as MSB
            let widths: Vec<u32> = vec![1; width as usize];
            let t_reversed: Vec<_> = t_bits.into_iter().rev().collect();
            let f_reversed: Vec<_> = f_bits.into_iter().rev().collect();

            self.alloc_node(
                LirOp::Concat {
                    widths: widths.clone(),
                },
                t_reversed,
                dest.t,
            );
            self.alloc_node(LirOp::Concat { widths }, f_reversed, dest.f);
        }
    }

    /// Expand an NCL multiplier using opaque NclMul operation.
    ///
    /// Uses the same opaque approach as expand_add_opaque for proper NULL/DATA handling.
    /// The NclMul operation takes all 4 dual-rail inputs (a_t, a_f, b_t, b_f) and
    /// produces properly-encoded interleaved output that respects NCL phase transitions.
    fn expand_mul(
        &mut self,
        a: DualRailPair,
        b: DualRailPair,
        dest: DualRailPair,
        input_width: u32,
        result_width: u32,
    ) {
        let id = self.next_signal_id;
        self.next_signal_id += 1;

        // Create interleaved output signal (2 * result_width bits)
        let interleaved =
            self.alloc_signal(format!("ncl_mul_interleaved_{}", id), result_width * 2);

        // Emit NclMul: inputs are a_t, a_f, b_t, b_f (each input_width bits)
        // Output is interleaved: [t0, f0, t1, f1, ...] for result_width bits
        // Use unique path to avoid cell name collisions when multiple MUL ops exist
        self.alloc_node_with_path(
            LirOp::NclMul {
                input_width,
                result_width,
            },
            vec![a.t, a.f, b.t, b.f],
            interleaved,
            format!("ncl_mul_{}", id),
        );

        // Deinterleave: extract t and f rails from interleaved output
        self.deinterleave_to_pair(interleaved, dest, result_width);
    }

    /// Expand an NCL less-than comparator using opaque NclLt operation.
    /// The opaque approach takes all dual-rail inputs and produces properly-encoded output
    /// that respects NULL/DATA phase transitions.
    fn expand_lt(&mut self, a: DualRailPair, b: DualRailPair, dest: DualRailPair, width: u32) {
        let id = self.next_signal_id;
        self.next_signal_id += 1;

        // Create interleaved output signal (2 bits: t, f for 1-bit result)
        let interleaved = self.alloc_signal(format!("ncl_lt_interleaved_{}", id), 2);

        // Emit NclLt: inputs are a_t, a_f, b_t, b_f (each width bits)
        // Output is interleaved: [t, f] for 1-bit result
        // Use unique path to avoid net name collision during flattening
        self.alloc_node_with_path(
            LirOp::NclLt { width },
            vec![a.t, a.f, b.t, b.f],
            interleaved,
            format!("ncl_lt_{}", id),
        );

        // Deinterleave: extract t and f rails from interleaved output
        self.alloc_node(
            LirOp::RangeSelect {
                width: 2,
                high: 0,
                low: 0,
            },
            vec![interleaved],
            dest.t,
        );
        self.alloc_node(
            LirOp::RangeSelect {
                width: 2,
                high: 1,
                low: 1,
            },
            vec![interleaved],
            dest.f,
        );
    }

    /// Expand an NCL equality comparator using proper dual-rail encoding
    /// For multi-bit equality: all bits must be equal for overall equality
    /// 1. Per-bit equality using TH22/TH12
    /// 2. AND-reduce eq_t bits (all must be equal)
    /// 3. OR-reduce eq_f bits (any difference means not equal)
    fn expand_eq(&mut self, a: DualRailPair, b: DualRailPair, dest: DualRailPair, width: u32) {
        if width == 1 {
            // Single-bit equality - simpler case
            // eq_t = TH12(TH22(a_t, b_t), TH22(a_f, b_f)) - both same
            // eq_f = TH12(TH22(a_t, b_f), TH22(a_f, b_t)) - different
            let both_true = self.alloc_signal(format!("eq_both_true_{}", self.next_signal_id), 1);
            let both_false = self.alloc_signal(format!("eq_both_false_{}", self.next_signal_id), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a.t, b.t], both_true);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a.f, b.f], both_false);
            self.alloc_node(
                LirOp::Th12 { width: 1 },
                vec![both_true, both_false],
                dest.t,
            );

            let diff_tf = self.alloc_signal(format!("eq_diff_tf_{}", self.next_signal_id), 1);
            let diff_ft = self.alloc_signal(format!("eq_diff_ft_{}", self.next_signal_id), 1);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a.t, b.f], diff_tf);
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![a.f, b.t], diff_ft);
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![diff_tf, diff_ft], dest.f);
        } else {
            // Multi-bit equality - compute per-bit then reduce
            // Per-bit equality signals (width bits each)
            let per_bit_eq_t =
                self.alloc_signal(format!("eq_perbit_t_{}", self.next_signal_id), width);
            let per_bit_eq_f =
                self.alloc_signal(format!("eq_perbit_f_{}", self.next_signal_id), width);

            // Intermediate signals for per-bit equality
            let both_true =
                self.alloc_signal(format!("eq_both_true_{}", self.next_signal_id), width);
            let both_false =
                self.alloc_signal(format!("eq_both_false_{}", self.next_signal_id), width);
            let diff_tf = self.alloc_signal(format!("eq_diff_tf_{}", self.next_signal_id), width);
            let diff_ft = self.alloc_signal(format!("eq_diff_ft_{}", self.next_signal_id), width);

            // Per-bit: eq_t[i] = TH12(TH22(a_t[i], b_t[i]), TH22(a_f[i], b_f[i]))
            self.alloc_node(LirOp::Th22 { width }, vec![a.t, b.t], both_true);
            self.alloc_node(LirOp::Th22 { width }, vec![a.f, b.f], both_false);
            self.alloc_node(
                LirOp::Th12 { width },
                vec![both_true, both_false],
                per_bit_eq_t,
            );

            // Per-bit: eq_f[i] = TH12(TH22(a_t[i], b_f[i]), TH22(a_f[i], b_t[i]))
            self.alloc_node(LirOp::Th22 { width }, vec![a.t, b.f], diff_tf);
            self.alloc_node(LirOp::Th22 { width }, vec![a.f, b.t], diff_ft);
            self.alloc_node(LirOp::Th12 { width }, vec![diff_tf, diff_ft], per_bit_eq_f);

            // Reduce using NCL threshold gates for proper hysteresis:
            // - AND reduction uses TH22 chain (C-element chain)
            // - OR reduction uses TH12 chain
            // Note: dest is 1-bit dual-rail output
            self.reduce_with_th22_chain(per_bit_eq_t, dest.t, width);
            self.reduce_with_th12_chain(per_bit_eq_f, dest.f, width);
        }
    }

    /// Reduce a multi-bit signal to 1-bit using TH22 chain (NCL AND reduction)
    fn reduce_with_th22_chain(&mut self, signal: LirSignalId, output: LirSignalId, width: u32) {
        if width == 1 {
            // Just buffer
            self.alloc_node(LirOp::Buf { width: 1 }, vec![signal], output);
        } else if width == 2 {
            // Extract bits and TH22 them
            let bit0 = self.alloc_signal(format!("red_and_b0_{}", self.next_signal_id), 1);
            let bit1 = self.alloc_signal(format!("red_and_b1_{}", self.next_signal_id), 1);
            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: 0,
                    low: 0,
                },
                vec![signal],
                bit0,
            );
            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: 1,
                    low: 1,
                },
                vec![signal],
                bit1,
            );
            self.alloc_node(LirOp::Th22 { width: 1 }, vec![bit0, bit1], output);
        } else {
            // Chain: reduce pairs with TH22, then recursively reduce the result
            let half = width / 2;
            let remaining = width - half;

            // Extract and reduce first half
            let first_half =
                self.alloc_signal(format!("red_and_half0_{}", self.next_signal_id), half);
            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: (half - 1),
                    low: 0,
                },
                vec![signal],
                first_half,
            );
            let first_reduced =
                self.alloc_signal(format!("red_and_h0r_{}", self.next_signal_id), 1);
            self.reduce_with_th22_chain(first_half, first_reduced, half);

            // Extract and reduce second half
            let second_half =
                self.alloc_signal(format!("red_and_half1_{}", self.next_signal_id), remaining);
            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: (width - 1),
                    low: half,
                },
                vec![signal],
                second_half,
            );
            let second_reduced =
                self.alloc_signal(format!("red_and_h1r_{}", self.next_signal_id), 1);
            self.reduce_with_th22_chain(second_half, second_reduced, remaining);

            // TH22 the two halves
            self.alloc_node(
                LirOp::Th22 { width: 1 },
                vec![first_reduced, second_reduced],
                output,
            );
        }
    }

    /// Reduce a multi-bit signal to 1-bit using TH12 chain (NCL OR reduction)
    fn reduce_with_th12_chain(&mut self, signal: LirSignalId, output: LirSignalId, width: u32) {
        if width == 1 {
            // Just buffer
            self.alloc_node(LirOp::Buf { width: 1 }, vec![signal], output);
        } else if width == 2 {
            // Extract bits and TH12 them
            let bit0 = self.alloc_signal(format!("red_or_b0_{}", self.next_signal_id), 1);
            let bit1 = self.alloc_signal(format!("red_or_b1_{}", self.next_signal_id), 1);
            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: 0,
                    low: 0,
                },
                vec![signal],
                bit0,
            );
            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: 1,
                    low: 1,
                },
                vec![signal],
                bit1,
            );
            self.alloc_node(LirOp::Th12 { width: 1 }, vec![bit0, bit1], output);
        } else {
            // Chain: reduce pairs with TH12, then recursively reduce the result
            let half = width / 2;
            let remaining = width - half;

            // Extract and reduce first half
            let first_half =
                self.alloc_signal(format!("red_or_half0_{}", self.next_signal_id), half);
            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: (half - 1),
                    low: 0,
                },
                vec![signal],
                first_half,
            );
            let first_reduced = self.alloc_signal(format!("red_or_h0r_{}", self.next_signal_id), 1);
            self.reduce_with_th12_chain(first_half, first_reduced, half);

            // Extract and reduce second half
            let second_half =
                self.alloc_signal(format!("red_or_half1_{}", self.next_signal_id), remaining);
            self.alloc_node(
                LirOp::RangeSelect {
                    width,
                    high: (width - 1),
                    low: half,
                },
                vec![signal],
                second_half,
            );
            let second_reduced =
                self.alloc_signal(format!("red_or_h1r_{}", self.next_signal_id), 1);
            self.reduce_with_th12_chain(second_half, second_reduced, remaining);

            // TH12 the two halves
            self.alloc_node(
                LirOp::Th12 { width: 1 },
                vec![first_reduced, second_reduced],
                output,
            );
        }
    }

    /// Expand an NCL 2:1 multiplexer using threshold gates
    /// MUX(sel, a, b) = (sel AND b) OR (NOT(sel) AND a)
    /// In NCL dual-rail:
    ///   y_t = TH12(TH22(sel_t, b_t), TH22(sel_f, a_t))
    ///   y_f = TH12(TH22(sel_t, b_f), TH22(sel_f, a_f))
    ///
    /// Note: sel may be 1-bit while data (a, b) may be multi-bit.
    /// When sel is 1-bit and data is wider, we process bit-by-bit.
    fn expand_mux2(
        &mut self,
        sel: DualRailPair,
        a: DualRailPair,
        b: DualRailPair,
        dest: DualRailPair,
        width: u32,
    ) {
        // Get the width of the select signal
        let sel_width = self.lir.signals[sel.t.0 as usize].width;

        // If select is 1-bit and data is multi-bit, use bit-by-bit MUX with 1-bit select
        if sel_width == 1 && width > 1 {
            // Create intermediate signals for gathering per-bit results
            let mut t_bits = Vec::with_capacity(width as usize);
            let mut f_bits = Vec::with_capacity(width as usize);

            for i in 0..width {
                // Extract bit i from a and b
                let a_t_bit =
                    self.alloc_signal(format!("mux_a_t_bit_{}_{}", self.next_signal_id, i), 1);
                let a_f_bit =
                    self.alloc_signal(format!("mux_a_f_bit_{}_{}", self.next_signal_id, i), 1);
                let b_t_bit =
                    self.alloc_signal(format!("mux_b_t_bit_{}_{}", self.next_signal_id, i), 1);
                let b_f_bit =
                    self.alloc_signal(format!("mux_b_f_bit_{}_{}", self.next_signal_id, i), 1);

                self.alloc_node(
                    LirOp::RangeSelect {
                        width,
                        high: i,
                        low: i,
                    },
                    vec![a.t],
                    a_t_bit,
                );
                self.alloc_node(
                    LirOp::RangeSelect {
                        width,
                        high: i,
                        low: i,
                    },
                    vec![a.f],
                    a_f_bit,
                );
                self.alloc_node(
                    LirOp::RangeSelect {
                        width,
                        high: i,
                        low: i,
                    },
                    vec![b.t],
                    b_t_bit,
                );
                self.alloc_node(
                    LirOp::RangeSelect {
                        width,
                        high: i,
                        low: i,
                    },
                    vec![b.f],
                    b_f_bit,
                );

                // Create 1-bit MUX for this bit
                let bit_t =
                    self.alloc_signal(format!("mux_bit_t_{}_{}", self.next_signal_id, i), 1);
                let bit_f =
                    self.alloc_signal(format!("mux_bit_f_{}_{}", self.next_signal_id, i), 1);

                // Intermediate signals for 1-bit MUX
                let sel_b_t =
                    self.alloc_signal(format!("mux1_sel_b_t_{}_{}", self.next_signal_id, i), 1);
                let nsel_a_t =
                    self.alloc_signal(format!("mux1_nsel_a_t_{}_{}", self.next_signal_id, i), 1);
                let sel_b_f =
                    self.alloc_signal(format!("mux1_sel_b_f_{}_{}", self.next_signal_id, i), 1);
                let nsel_a_f =
                    self.alloc_signal(format!("mux1_nsel_a_f_{}_{}", self.next_signal_id, i), 1);

                // TH22(sel_t, b_t[i]) for true rail
                self.alloc_node(LirOp::Th22 { width: 1 }, vec![sel.t, b_t_bit], sel_b_t);
                self.alloc_node(LirOp::Th22 { width: 1 }, vec![sel.f, a_t_bit], nsel_a_t);
                self.alloc_node(LirOp::Th12 { width: 1 }, vec![sel_b_t, nsel_a_t], bit_t);

                // TH22(sel_t, b_f[i]) for false rail
                self.alloc_node(LirOp::Th22 { width: 1 }, vec![sel.t, b_f_bit], sel_b_f);
                self.alloc_node(LirOp::Th22 { width: 1 }, vec![sel.f, a_f_bit], nsel_a_f);
                self.alloc_node(LirOp::Th12 { width: 1 }, vec![sel_b_f, nsel_a_f], bit_f);

                t_bits.push(bit_t);
                f_bits.push(bit_f);
            }

            // Concatenate all bits back together
            // Note: Concat expects inputs in MSB-first order (last input becomes LSB)
            // So we reverse the bit vectors to get correct bit ordering
            let widths: Vec<u32> = vec![1; width as usize];
            let t_bits_rev: Vec<_> = t_bits.into_iter().rev().collect();
            let f_bits_rev: Vec<_> = f_bits.into_iter().rev().collect();
            self.alloc_node(
                LirOp::Concat {
                    widths: widths.clone(),
                },
                t_bits_rev,
                dest.t,
            );
            self.alloc_node(LirOp::Concat { widths }, f_bits_rev, dest.f);
        } else {
            // Same-width case: use direct TH22/TH12 on full width
            // Intermediate signals for true rail
            let sel_b_t = self.alloc_signal(format!("mux_sel_b_t_{}", self.next_signal_id), width);
            let nsel_a_t =
                self.alloc_signal(format!("mux_nsel_a_t_{}", self.next_signal_id), width);

            // TH22(sel_t, b_t) - select high AND b true
            self.alloc_node(LirOp::Th22 { width }, vec![sel.t, b.t], sel_b_t);
            // TH22(sel_f, a_t) - select low AND a true (sel_f is NOT sel in dual-rail)
            self.alloc_node(LirOp::Th22 { width }, vec![sel.f, a.t], nsel_a_t);
            // True rail: TH12(sel_b_t, nsel_a_t) - OR with hysteresis
            self.alloc_node(LirOp::Th12 { width }, vec![sel_b_t, nsel_a_t], dest.t);

            // Intermediate signals for false rail
            let sel_b_f = self.alloc_signal(format!("mux_sel_b_f_{}", self.next_signal_id), width);
            let nsel_a_f =
                self.alloc_signal(format!("mux_nsel_a_f_{}", self.next_signal_id), width);

            // TH22(sel_t, b_f) - select high AND b false
            self.alloc_node(LirOp::Th22 { width }, vec![sel.t, b.f], sel_b_f);
            // TH22(sel_f, a_f) - select low AND a false
            self.alloc_node(LirOp::Th22 { width }, vec![sel.f, a.f], nsel_a_f);
            // False rail: TH12(sel_b_f, nsel_a_f) - OR with hysteresis
            self.alloc_node(LirOp::Th12 { width }, vec![sel_b_f, nsel_a_f], dest.f);
        }
    }

    /// Expand a register to NCL register (NULL/DATA latch)
    /// Simplified approach: use regular register, both rails get same input treated as t/f
    fn expand_reg(&mut self, d: DualRailPair, dest: DualRailPair, width: u32) {
        // Simplified approach: register both rails separately
        // This is an approximation - proper NCL register needs special handling
        self.alloc_node(
            LirOp::Reg {
                width,
                has_enable: false,
                has_reset: false,
                reset_value: None,
            },
            vec![d.t],
            dest.t,
        );
        self.alloc_node(
            LirOp::Reg {
                width,
                has_enable: false,
                has_reset: false,
                reset_value: None,
            },
            vec![d.f],
            dest.f,
        );
    }

    /// Expand a constant to NCL dual-rail encoding
    /// For constant value v with width w:
    /// - t rail has bits set where v has 1s (DATA_TRUE)
    /// - f rail has bits set where v has 0s (DATA_FALSE)
    fn expand_constant(&mut self, value: u64, dest: DualRailPair, width: u32) {
        // Compute the mask for the width
        let mask = if width >= 64 {
            u64::MAX
        } else {
            (1u64 << width) - 1
        };

        // t rail: bits that are 1 in the constant
        self.alloc_node(LirOp::Constant { width, value }, vec![], dest.t);

        // f rail: bits that are 0 in the constant (inverted)
        let f_value = (!value) & mask;
        self.alloc_node(
            LirOp::Constant {
                width,
                value: f_value,
            },
            vec![],
            dest.f,
        );
    }

    /// BUG #184 FIX: Expand ZeroExtend to NCL dual-rail encoding
    /// Lower bits are copied from input, upper bits are padded with NCL DATA_FALSE (t=0, f=1)
    fn expand_zero_extend(&mut self, input: DualRailPair, dest: DualRailPair, from: u32, to: u32) {
        // Lower `from` bits: use RangeSelect to extract and buffer
        if from > 0 {
            // Extract lower bits from true rail
            let low_t = self.alloc_signal(format!("zext_low_t_{}", self.next_signal_id), from);
            self.alloc_node(
                LirOp::RangeSelect {
                    width: from,
                    high: from - 1,
                    low: 0,
                },
                vec![input.t],
                low_t,
            );

            // Extract lower bits from false rail
            let low_f = self.alloc_signal(format!("zext_low_f_{}", self.next_signal_id), from);
            self.alloc_node(
                LirOp::RangeSelect {
                    width: from,
                    high: from - 1,
                    low: 0,
                },
                vec![input.f],
                low_f,
            );

            // Upper bits: NCL DATA_FALSE (t=0, f=1)
            let upper_width = to - from;
            let upper_t =
                self.alloc_signal(format!("zext_upper_t_{}", self.next_signal_id), upper_width);
            let upper_f =
                self.alloc_signal(format!("zext_upper_f_{}", self.next_signal_id), upper_width);
            self.alloc_node(
                LirOp::Constant {
                    width: upper_width,
                    value: 0,
                },
                vec![],
                upper_t,
            );
            // For NCL DATA_FALSE, f rail should be all 1s
            let upper_f_mask = if upper_width >= 64 {
                u64::MAX
            } else {
                (1u64 << upper_width) - 1
            };
            self.alloc_node(
                LirOp::Constant {
                    width: upper_width,
                    value: upper_f_mask,
                },
                vec![],
                upper_f,
            );

            // Concatenate: [upper, lower] for both rails
            let widths = vec![upper_width, from];
            self.alloc_node(
                LirOp::Concat {
                    widths: widths.clone(),
                },
                vec![upper_t, low_t],
                dest.t,
            );
            self.alloc_node(LirOp::Concat { widths }, vec![upper_f, low_f], dest.f);
        } else {
            // from == 0: All bits are zero-padded (just a constant 0)
            self.alloc_node(
                LirOp::Constant {
                    width: to,
                    value: 0,
                },
                vec![],
                dest.t,
            );
            let f_mask = if to >= 64 { u64::MAX } else { (1u64 << to) - 1 };
            self.alloc_node(
                LirOp::Constant {
                    width: to,
                    value: f_mask,
                },
                vec![],
                dest.f,
            );
        }
    }

    /// BUG #184 FIX: Expand SignExtend to NCL dual-rail encoding
    /// Lower bits are copied from input, upper bits replicate the sign bit
    fn expand_sign_extend(&mut self, input: DualRailPair, dest: DualRailPair, from: u32, to: u32) {
        if from > 0 {
            // Extract lower bits from both rails
            let low_t = self.alloc_signal(format!("sext_low_t_{}", self.next_signal_id), from);
            self.alloc_node(
                LirOp::RangeSelect {
                    width: from,
                    high: from - 1,
                    low: 0,
                },
                vec![input.t],
                low_t,
            );

            let low_f = self.alloc_signal(format!("sext_low_f_{}", self.next_signal_id), from);
            self.alloc_node(
                LirOp::RangeSelect {
                    width: from,
                    high: from - 1,
                    low: 0,
                },
                vec![input.f],
                low_f,
            );

            // Extract sign bit (highest bit of input)
            let sign_bit_t = self.alloc_signal(format!("sext_sign_t_{}", self.next_signal_id), 1);
            self.alloc_node(
                LirOp::RangeSelect {
                    width: from,
                    high: from - 1,
                    low: from - 1,
                },
                vec![input.t],
                sign_bit_t,
            );

            let sign_bit_f = self.alloc_signal(format!("sext_sign_f_{}", self.next_signal_id), 1);
            self.alloc_node(
                LirOp::RangeSelect {
                    width: from,
                    high: from - 1,
                    low: from - 1,
                },
                vec![input.f],
                sign_bit_f,
            );

            // Replicate sign bit for upper bits
            let upper_width = to - from;
            let mut upper_t_bits = Vec::new();
            let mut upper_f_bits = Vec::new();

            for _ in 0..upper_width {
                upper_t_bits.push(sign_bit_t);
                upper_f_bits.push(sign_bit_f);
            }

            // If upper_width > 1, we need to concat the replicated sign bits
            if upper_width > 0 {
                let upper_t =
                    self.alloc_signal(format!("sext_upper_t_{}", self.next_signal_id), upper_width);
                let upper_f =
                    self.alloc_signal(format!("sext_upper_f_{}", self.next_signal_id), upper_width);

                let widths: Vec<u32> = vec![1; upper_width as usize];
                self.alloc_node(
                    LirOp::Concat {
                        widths: widths.clone(),
                    },
                    upper_t_bits,
                    upper_t,
                );
                self.alloc_node(LirOp::Concat { widths }, upper_f_bits, upper_f);

                // Concatenate: [upper, lower] for both rails
                let final_widths = vec![upper_width, from];
                self.alloc_node(
                    LirOp::Concat {
                        widths: final_widths.clone(),
                    },
                    vec![upper_t, low_t],
                    dest.t,
                );
                self.alloc_node(
                    LirOp::Concat {
                        widths: final_widths,
                    },
                    vec![upper_f, low_f],
                    dest.f,
                );
            } else {
                // No extension needed, just copy
                self.alloc_node(LirOp::Buf { width: from }, vec![low_t], dest.t);
                self.alloc_node(LirOp::Buf { width: from }, vec![low_f], dest.f);
            }
        } else {
            // from == 0: All bits are zero (undefined behavior, just output zeros)
            self.alloc_node(
                LirOp::Constant {
                    width: to,
                    value: 0,
                },
                vec![],
                dest.t,
            );
            let f_mask = if to >= 64 { u64::MAX } else { (1u64 << to) - 1 };
            self.alloc_node(
                LirOp::Constant {
                    width: to,
                    value: f_mask,
                },
                vec![],
                dest.f,
            );
        }
    }

    /// Expand a left shift to NCL using THmn-based barrel shifter
    /// Each stage uses NCL mux chains with proper dual-rail encoding
    fn expand_shl(&mut self, a: DualRailPair, shamt: DualRailPair, dest: DualRailPair, width: u32) {
        // Calculate number of shift stages (log2 of width)
        let shift_bits = if width <= 1 {
            1
        } else {
            (32 - (width - 1).leading_zeros()) as usize
        };

        // Create constant 0 dual-rail pair for filling in shifted bits
        // In NCL, 0 = (t=0, f=1), so we use a constant signal
        let const_0_t = self.alloc_signal(format!("shl_const0_t_{}", self.next_signal_id), 1);
        let const_0_f = self.alloc_signal(format!("shl_const0_f_{}", self.next_signal_id), 1);
        self.alloc_node(LirOp::Constant { width: 1, value: 0 }, vec![], const_0_t);
        self.alloc_node(LirOp::Constant { width: 1, value: 1 }, vec![], const_0_f);
        let zero_pair = DualRailPair {
            t: const_0_t,
            f: const_0_f,
        };

        // Start with input bits
        let mut current: Vec<DualRailPair> = (0..width)
            .map(|i| {
                let bit_t = self.alloc_signal(format!("shl_in_t_{}_{}", self.next_signal_id, i), 1);
                let bit_f = self.alloc_signal(format!("shl_in_f_{}_{}", self.next_signal_id, i), 1);
                // Extract bit i from a.t and a.f
                self.alloc_node(
                    LirOp::RangeSelect {
                        width,
                        high: i,
                        low: i,
                    },
                    vec![a.t],
                    bit_t,
                );
                self.alloc_node(
                    LirOp::RangeSelect {
                        width,
                        high: i,
                        low: i,
                    },
                    vec![a.f],
                    bit_f,
                );
                DualRailPair { t: bit_t, f: bit_f }
            })
            .collect();

        // Build barrel shifter with cascaded NCL mux stages
        for stage in 0..shift_bits {
            let shift_amount = 1usize << stage;

            // Extract shift amount bit for this stage
            let sel_t =
                self.alloc_signal(format!("shl_sel_t_{}_{}", self.next_signal_id, stage), 1);
            let sel_f =
                self.alloc_signal(format!("shl_sel_f_{}_{}", self.next_signal_id, stage), 1);

            // Get the shift amount bit (or use 0 if beyond width)
            let shamt_width = self.lir.signals[shamt.t.0 as usize].width;
            if (stage as u32) < shamt_width {
                self.alloc_node(
                    LirOp::RangeSelect {
                        width: shamt_width,
                        high: stage as u32,
                        low: stage as u32,
                    },
                    vec![shamt.t],
                    sel_t,
                );
                self.alloc_node(
                    LirOp::RangeSelect {
                        width: shamt_width,
                        high: stage as u32,
                        low: stage as u32,
                    },
                    vec![shamt.f],
                    sel_f,
                );
            } else {
                // Beyond shift amount width, shift bit is 0
                self.alloc_node(LirOp::Constant { width: 1, value: 0 }, vec![], sel_t);
                self.alloc_node(LirOp::Constant { width: 1, value: 1 }, vec![], sel_f);
            }
            let sel_pair = DualRailPair { t: sel_t, f: sel_f };

            let mut next_stage: Vec<DualRailPair> = Vec::with_capacity(width as usize);

            for bit in 0..width as usize {
                // Calculate source bit for shifted value
                // Left shift: when sel=1, bit[i] gets bit[i-shift_amount] (or 0)
                let unshifted = current[bit];
                let shifted = if bit >= shift_amount {
                    current[bit - shift_amount]
                } else {
                    zero_pair
                };

                // Create output for this bit at this stage
                let out_t = self.alloc_signal(
                    format!("shl_s{}_b{}_t_{}", stage, bit, self.next_signal_id),
                    1,
                );
                let out_f = self.alloc_signal(
                    format!("shl_s{}_b{}_f_{}", stage, bit, self.next_signal_id),
                    1,
                );
                let out_pair = DualRailPair { t: out_t, f: out_f };

                // NCL MUX: sel=0 -> unshifted, sel=1 -> shifted
                self.expand_mux2_1bit(sel_pair, unshifted, shifted, out_pair);

                next_stage.push(out_pair);
            }

            current = next_stage;
        }

        // Concatenate final bits back to output
        let t_bits: Vec<LirSignalId> = current.iter().rev().map(|p| p.t).collect();
        let f_bits: Vec<LirSignalId> = current.iter().rev().map(|p| p.f).collect();
        let widths: Vec<u32> = vec![1; width as usize];
        self.alloc_node(
            LirOp::Concat {
                widths: widths.clone(),
            },
            t_bits,
            dest.t,
        );
        self.alloc_node(LirOp::Concat { widths }, f_bits, dest.f);
    }

    /// Expand a right shift to NCL using THmn-based barrel shifter
    fn expand_shr(&mut self, a: DualRailPair, shamt: DualRailPair, dest: DualRailPair, width: u32) {
        // Calculate number of shift stages (log2 of width)
        let shift_bits = if width <= 1 {
            1
        } else {
            (32 - (width - 1).leading_zeros()) as usize
        };

        // Create constant 0 dual-rail pair for filling in shifted bits
        let const_0_t = self.alloc_signal(format!("shr_const0_t_{}", self.next_signal_id), 1);
        let const_0_f = self.alloc_signal(format!("shr_const0_f_{}", self.next_signal_id), 1);
        self.alloc_node(LirOp::Constant { width: 1, value: 0 }, vec![], const_0_t);
        self.alloc_node(LirOp::Constant { width: 1, value: 1 }, vec![], const_0_f);
        let zero_pair = DualRailPair {
            t: const_0_t,
            f: const_0_f,
        };

        // Start with input bits
        let mut current: Vec<DualRailPair> = (0..width)
            .map(|i| {
                let bit_t = self.alloc_signal(format!("shr_in_t_{}_{}", self.next_signal_id, i), 1);
                let bit_f = self.alloc_signal(format!("shr_in_f_{}_{}", self.next_signal_id, i), 1);
                self.alloc_node(
                    LirOp::RangeSelect {
                        width,
                        high: i,
                        low: i,
                    },
                    vec![a.t],
                    bit_t,
                );
                self.alloc_node(
                    LirOp::RangeSelect {
                        width,
                        high: i,
                        low: i,
                    },
                    vec![a.f],
                    bit_f,
                );
                DualRailPair { t: bit_t, f: bit_f }
            })
            .collect();

        // Build barrel shifter with cascaded NCL mux stages
        for stage in 0..shift_bits {
            let shift_amount = 1usize << stage;

            // Extract shift amount bit for this stage
            let sel_t =
                self.alloc_signal(format!("shr_sel_t_{}_{}", self.next_signal_id, stage), 1);
            let sel_f =
                self.alloc_signal(format!("shr_sel_f_{}_{}", self.next_signal_id, stage), 1);

            let shamt_width = self.lir.signals[shamt.t.0 as usize].width;
            if (stage as u32) < shamt_width {
                self.alloc_node(
                    LirOp::RangeSelect {
                        width: shamt_width,
                        high: stage as u32,
                        low: stage as u32,
                    },
                    vec![shamt.t],
                    sel_t,
                );
                self.alloc_node(
                    LirOp::RangeSelect {
                        width: shamt_width,
                        high: stage as u32,
                        low: stage as u32,
                    },
                    vec![shamt.f],
                    sel_f,
                );
            } else {
                self.alloc_node(LirOp::Constant { width: 1, value: 0 }, vec![], sel_t);
                self.alloc_node(LirOp::Constant { width: 1, value: 1 }, vec![], sel_f);
            }
            let sel_pair = DualRailPair { t: sel_t, f: sel_f };

            let mut next_stage: Vec<DualRailPair> = Vec::with_capacity(width as usize);

            for bit in 0..width as usize {
                // Right shift: when sel=1, bit[i] gets bit[i+shift_amount] (or 0)
                let unshifted = current[bit];
                let shifted = if bit + shift_amount < width as usize {
                    current[bit + shift_amount]
                } else {
                    zero_pair
                };

                let out_t = self.alloc_signal(
                    format!("shr_s{}_b{}_t_{}", stage, bit, self.next_signal_id),
                    1,
                );
                let out_f = self.alloc_signal(
                    format!("shr_s{}_b{}_f_{}", stage, bit, self.next_signal_id),
                    1,
                );
                let out_pair = DualRailPair { t: out_t, f: out_f };

                // NCL MUX: sel=0 -> unshifted, sel=1 -> shifted
                self.expand_mux2_1bit(sel_pair, unshifted, shifted, out_pair);

                next_stage.push(out_pair);
            }

            current = next_stage;
        }

        // Concatenate final bits back to output
        let t_bits: Vec<LirSignalId> = current.iter().rev().map(|p| p.t).collect();
        let f_bits: Vec<LirSignalId> = current.iter().rev().map(|p| p.f).collect();
        let widths: Vec<u32> = vec![1; width as usize];
        self.alloc_node(
            LirOp::Concat {
                widths: widths.clone(),
            },
            t_bits,
            dest.t,
        );
        self.alloc_node(LirOp::Concat { widths }, f_bits, dest.f);
    }

    /// 1-bit NCL MUX using THmn gates
    /// y = (sel AND b) OR (NOT sel AND a)
    /// In NCL dual-rail:
    ///   y_t = TH12(TH22(sel_t, b_t), TH22(sel_f, a_t))
    ///   y_f = TH12(TH22(sel_t, b_f), TH22(sel_f, a_f))
    fn expand_mux2_1bit(
        &mut self,
        sel: DualRailPair,
        a: DualRailPair,
        b: DualRailPair,
        dest: DualRailPair,
    ) {
        // Intermediate signals for true rail
        let sel_b_t = self.alloc_signal(format!("mux1_sel_b_t_{}", self.next_signal_id), 1);
        let nsel_a_t = self.alloc_signal(format!("mux1_nsel_a_t_{}", self.next_signal_id), 1);

        // TH22(sel_t, b_t) - select high AND b true
        self.alloc_node(LirOp::Th22 { width: 1 }, vec![sel.t, b.t], sel_b_t);
        // TH22(sel_f, a_t) - select low AND a true
        self.alloc_node(LirOp::Th22 { width: 1 }, vec![sel.f, a.t], nsel_a_t);
        // True rail: TH12(sel_b_t, nsel_a_t) - OR with hysteresis
        self.alloc_node(LirOp::Th12 { width: 1 }, vec![sel_b_t, nsel_a_t], dest.t);

        // Intermediate signals for false rail
        let sel_b_f = self.alloc_signal(format!("mux1_sel_b_f_{}", self.next_signal_id), 1);
        let nsel_a_f = self.alloc_signal(format!("mux1_nsel_a_f_{}", self.next_signal_id), 1);

        // TH22(sel_t, b_f) - select high AND b false
        self.alloc_node(LirOp::Th22 { width: 1 }, vec![sel.t, b.f], sel_b_f);
        // TH22(sel_f, a_f) - select low AND a false
        self.alloc_node(LirOp::Th22 { width: 1 }, vec![sel.f, a.f], nsel_a_f);
        // False rail: TH12(sel_b_f, nsel_a_f) - OR with hysteresis
        self.alloc_node(LirOp::Th12 { width: 1 }, vec![sel_b_f, nsel_a_f], dest.f);
    }

    /// Encode a single-rail input to dual-rail
    fn encode_input(&mut self, signal: LirSignalId, width: u32) -> DualRailPair {
        let t = self.alloc_signal(
            format!("{}_t", self.lir.signals[signal.0 as usize].name),
            width,
        );
        let f = self.alloc_signal(
            format!("{}_f", self.lir.signals[signal.0 as usize].name),
            width,
        );

        self.alloc_node(LirOp::NclEncode { width }, vec![signal], t);

        DualRailPair { t, f }
    }

    /// Decode dual-rail to single-rail output
    fn decode_output(&mut self, pair: DualRailPair, width: u32) -> LirSignalId {
        let output = self.alloc_signal(format!("decoded_{}", self.next_signal_id), width);

        self.alloc_node(LirOp::NclDecode { width }, vec![pair.t, pair.f], output);

        output
    }
}

/// Expand a synchronous LIR to NCL dual-rail logic
///
/// # Arguments
/// * `lir` - The synchronous LIR to transform
/// * `config` - NCL expansion configuration
///
/// # Returns
/// * `NclExpandResult` containing the transformed NCL LIR
pub fn expand_to_ncl(lir: &Lir, config: &NclConfig) -> NclExpandResult {
    let mut expander = NclExpander::new(&lir.name, config.clone());

    // Debug: print original LIR structure
    eprintln!("=== NCL Expand: Original LIR ===");
    eprintln!("Signals ({}):", lir.signals.len());
    for sig in &lir.signals {
        eprintln!(
            "  {:?} '{}' w={} in={} out={}",
            sig.id, sig.name, sig.width, sig.is_input, sig.is_output
        );
    }
    eprintln!("Nodes ({}):", lir.nodes.len());
    for node in &lir.nodes {
        eprintln!(
            "  {:?} op={:?} inputs={:?} output={:?}",
            node.id, node.op, node.inputs, node.output
        );
    }
    eprintln!("Inputs: {:?}", lir.inputs);
    eprintln!("Outputs: {:?}", lir.outputs);
    eprintln!("================================");

    // Step 1: Create dual-rail signals for all original signals
    for signal in &lir.signals {
        let pair = expander.create_dual_rail(signal);
        expander.dual_rail_map.insert(signal.id, pair);
    }

    // Step 2: Mark inputs and create encoders if needed
    for &input_id in &lir.inputs {
        if let Some(&pair) = expander.dual_rail_map.get(&input_id) {
            // Mark both rails as inputs
            expander.lir.signals[pair.t.0 as usize].is_input = true;
            expander.lir.signals[pair.f.0 as usize].is_input = true;
            expander.lir.inputs.push(pair.t);
            expander.lir.inputs.push(pair.f);
        }
    }

    // Step 3: Process each node and expand to NCL equivalent
    for node in &lir.nodes {
        let output_pair = expander.dual_rail_map.get(&node.output).copied();

        match &node.op {
            LirOp::And { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.expand_and(a, b, dest, *width);
                    }
                }
            }
            LirOp::Or { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.expand_or(a, b, dest, *width);
                    }
                }
            }
            LirOp::Not { width } => {
                if !node.inputs.is_empty() {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    if let (Some(a), Some(dest)) = (a, output_pair) {
                        // Use the existing output dual-rail pair as destination
                        expander.expand_not(a, dest, *width);
                        // No need to update dual_rail_map - dest is already the output's pair
                    }
                }
            }
            LirOp::Xor { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.expand_xor(a, b, dest, *width);
                    }
                }
            }
            LirOp::Add { width, .. } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        if expander.config.use_opaque_arithmetic {
                            expander.expand_add_opaque(a, b, dest, *width);
                        } else {
                            expander.expand_add(a, b, dest, *width);
                        }
                    }
                }
            }
            LirOp::Sub { width, .. } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        if expander.config.use_opaque_arithmetic {
                            expander.expand_sub_opaque(a, b, dest, *width);
                        } else {
                            expander.expand_sub(a, b, dest, *width);
                        }
                    }
                }
            }
            LirOp::Mul { result_width, .. } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    // Get actual input width from the input signals
                    let input_width = lir.signals[node.inputs[0].0 as usize].width;
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.expand_mul(a, b, dest, input_width, *result_width);
                    }
                }
            }
            LirOp::Lt { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.expand_lt(a, b, dest, *width);
                    }
                }
            }
            LirOp::Eq { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.expand_eq(a, b, dest, *width);
                    }
                }
            }
            LirOp::Mux2 { width } => {
                if node.inputs.len() >= 3 {
                    let sel = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let a = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[2]).copied();
                    if let (Some(sel), Some(a), Some(b), Some(dest)) = (sel, a, b, output_pair) {
                        expander.expand_mux2(sel, a, b, dest, *width);
                    }
                }
            }
            LirOp::Reg { width, .. } => {
                if !node.inputs.is_empty() {
                    let d = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    if let (Some(d), Some(dest)) = (d, output_pair) {
                        expander.expand_reg(d, dest, *width);
                    }
                }
            }
            // Buffer operations - just copy dual-rail signals
            LirOp::Buf { width } | LirOp::Buffer { width } => {
                if !node.inputs.is_empty() {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    if let (Some(a), Some(dest)) = (a, output_pair) {
                        // Buffer copies both rails from input to output
                        expander.expand_buf(a, dest, *width);
                    }
                }
            }
            // Constants - create dual-rail encoding
            LirOp::Constant { width, value } => {
                if let Some(dest) = output_pair {
                    expander.expand_constant(*value, dest, *width);
                }
            }
            // Shifts - use THmn-based barrel shifter for proper NCL dual-rail semantics
            LirOp::Shl { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let shamt = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(shamt), Some(dest)) = (a, shamt, output_pair) {
                        expander.expand_shl(a, shamt, dest, *width);
                    }
                }
            }
            LirOp::Shr { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let shamt = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(shamt), Some(dest)) = (a, shamt, output_pair) {
                        expander.expand_shr(a, shamt, dest, *width);
                    }
                }
            }
            // RangeSelect - extract a range of bits from a signal
            LirOp::RangeSelect { width, high, low } => {
                if !node.inputs.is_empty() {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    if let (Some(a), Some(dest)) = (a, output_pair) {
                        // For dual-rail, RangeSelect extracts range from both rails
                        // Since we're using simplified NCL with interleaved bits, we pass through
                        expander.alloc_node(
                            LirOp::RangeSelect {
                                width: *width,
                                high: *high,
                                low: *low,
                            },
                            vec![a.t],
                            dest.t,
                        );
                        expander.alloc_node(
                            LirOp::RangeSelect {
                                width: *width,
                                high: *high,
                                low: *low,
                            },
                            vec![a.f],
                            dest.f,
                        );
                    }
                }
            }
            // Concat - concatenate multiple signals
            LirOp::Concat { widths } => {
                // Get input pairs for all inputs
                let input_pairs: Vec<Option<DualRailPair>> = node
                    .inputs
                    .iter()
                    .map(|id| expander.dual_rail_map.get(id).copied())
                    .collect();

                if input_pairs.iter().all(|p| p.is_some()) {
                    if let Some(dest) = output_pair {
                        // Collect true rails and false rails
                        let t_inputs: Vec<LirSignalId> = input_pairs
                            .iter()
                            .filter_map(|p| p.map(|pair| pair.t))
                            .collect();
                        let f_inputs: Vec<LirSignalId> = input_pairs
                            .iter()
                            .filter_map(|p| p.map(|pair| pair.f))
                            .collect();

                        expander.alloc_node(
                            LirOp::Concat {
                                widths: widths.clone(),
                            },
                            t_inputs,
                            dest.t,
                        );
                        expander.alloc_node(
                            LirOp::Concat {
                                widths: widths.clone(),
                            },
                            f_inputs,
                            dest.f,
                        );
                    }
                }
            }
            // Greater than or equal: ge = NOT(lt) - swap lt outputs
            // ge_t = lt_f (a >= b when NOT a < b)
            // ge_f = lt_t (a < b)
            LirOp::Ge { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        // Swap output rails: ge = NOT(lt)
                        let swapped_dest = DualRailPair {
                            t: dest.f,
                            f: dest.t,
                        };
                        expander.expand_lt(a, b, swapped_dest, *width);
                    }
                }
            }
            // Greater than: gt = lt with swapped inputs (a > b is b < a)
            LirOp::Gt { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        // Swap inputs: gt(a, b) = lt(b, a)
                        expander.expand_lt(b, a, dest, *width);
                    }
                }
            }
            // Less than or equal: le = NOT(gt) = NOT(lt with swapped inputs)
            // le_t = gt_f = lt(b,a)_f
            // le_f = gt_t = lt(b,a)_t
            LirOp::Le { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        // Swap both inputs and outputs: le(a, b) = NOT(lt(b, a))
                        let swapped_dest = DualRailPair {
                            t: dest.f,
                            f: dest.t,
                        };
                        expander.expand_lt(b, a, swapped_dest, *width);
                    }
                }
            }
            // Not equal: ne = NOT(eq) - swap eq outputs
            LirOp::Ne { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        // Swap output rails: ne = NOT(eq)
                        let swapped_dest = DualRailPair {
                            t: dest.f,
                            f: dest.t,
                        };
                        expander.expand_eq(a, b, swapped_dest, *width);
                    }
                }
            }
            // Floating-point operations - use true rails for computation, invert for false rail
            // These are soft macros that operate on bit representations
            LirOp::FpAdd { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        // Use true rails for FP computation
                        expander.alloc_node(LirOp::FpAdd { width: *width }, vec![a.t, b.t], dest.t);
                        expander.alloc_node(LirOp::Not { width: *width }, vec![dest.t], dest.f);
                    }
                }
            }
            LirOp::FpSub { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        // Use true rails for FP computation
                        expander.alloc_node(LirOp::FpSub { width: *width }, vec![a.t, b.t], dest.t);
                        expander.alloc_node(LirOp::Not { width: *width }, vec![dest.t], dest.f);
                    }
                }
            }
            LirOp::FpMul { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        // Use true rails for FP computation
                        expander.alloc_node(LirOp::FpMul { width: *width }, vec![a.t, b.t], dest.t);
                        expander.alloc_node(LirOp::Not { width: *width }, vec![dest.t], dest.f);
                    }
                }
            }
            LirOp::FpDiv { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        // Use true rails for FP computation
                        expander.alloc_node(LirOp::FpDiv { width: *width }, vec![a.t, b.t], dest.t);
                        expander.alloc_node(LirOp::Not { width: *width }, vec![dest.t], dest.f);
                    }
                }
            }
            // FP Comparisons - output is 1 bit (dual-rail)
            LirOp::FpLt { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        // Use true rails for FP comparison, result is 1 bit
                        expander.alloc_node(LirOp::FpLt { width: *width }, vec![a.t, b.t], dest.t);
                        expander.alloc_node(LirOp::Not { width: 1 }, vec![dest.t], dest.f);
                    }
                }
            }
            LirOp::FpGt { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.alloc_node(LirOp::FpGt { width: *width }, vec![a.t, b.t], dest.t);
                        expander.alloc_node(LirOp::Not { width: 1 }, vec![dest.t], dest.f);
                    }
                }
            }
            LirOp::FpLe { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.alloc_node(LirOp::FpLe { width: *width }, vec![a.t, b.t], dest.t);
                        expander.alloc_node(LirOp::Not { width: 1 }, vec![dest.t], dest.f);
                    }
                }
            }
            LirOp::FpGe { width } => {
                if node.inputs.len() >= 2 {
                    let a = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    let b = expander.dual_rail_map.get(&node.inputs[1]).copied();
                    if let (Some(a), Some(b), Some(dest)) = (a, b, output_pair) {
                        expander.alloc_node(LirOp::FpGe { width: *width }, vec![a.t, b.t], dest.t);
                        expander.alloc_node(LirOp::Not { width: 1 }, vec![dest.t], dest.f);
                    }
                }
            }
            // BUG #184 FIX: ZeroExtend - copy lower bits, pad upper bits with NCL DATA_FALSE (t=0, f=1)
            LirOp::ZeroExtend { from, to } => {
                if !node.inputs.is_empty() {
                    let input = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    if let (Some(input), Some(dest)) = (input, output_pair) {
                        expander.expand_zero_extend(input, dest, *from, *to);
                    }
                }
            }
            // BUG #184 FIX: SignExtend - copy lower bits, replicate sign bit to upper bits
            LirOp::SignExtend { from, to } => {
                if !node.inputs.is_empty() {
                    let input = expander.dual_rail_map.get(&node.inputs[0]).copied();
                    if let (Some(input), Some(dest)) = (input, output_pair) {
                        expander.expand_sign_extend(input, dest, *from, *to);
                    }
                }
            }
            // Handle other operations by passing through or generating placeholders
            _ => {
                // For unsupported operations, create pass-through
                // This will be expanded in later phases
            }
        }
    }

    // Step 4: Mark outputs
    for &output_id in &lir.outputs {
        if let Some(&pair) = expander.dual_rail_map.get(&output_id) {
            expander.lir.signals[pair.t.0 as usize].is_output = true;
            expander.lir.signals[pair.f.0 as usize].is_output = true;
            expander.lir.outputs.push(pair.t);
            expander.lir.outputs.push(pair.f);
        }
    }

    // Step 5: Create completion detection for outputs (weak completion)
    let mut stage_completions = Vec::new();
    if config.use_weak_completion {
        let output_pairs: Vec<DualRailPair> = lir
            .outputs
            .iter()
            .filter_map(|id| expander.dual_rail_map.get(id).copied())
            .collect();

        if !output_pairs.is_empty() {
            let completion = expander.create_completion(&output_pairs);
            stage_completions.push(completion);
        }
    }

    // Mark the LIR as NCL - this is the source of truth for NCL detection
    expander.lir.is_ncl = true;

    NclExpandResult {
        lir: expander.lir,
        dual_rail_map: expander.dual_rail_map,
        stage_completions,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ncl_config_default() {
        let config = NclConfig::default();
        assert!(config.use_weak_completion);
        assert!(config.completion_tree_depth.is_none());
        assert!(config.generate_null_wavefront);
    }

    #[test]
    fn test_dual_rail_creation() {
        let config = NclConfig::default();
        let mut expander = NclExpander::new("test", config);

        let original = LirSignal {
            id: LirSignalId(0),
            name: "data".to_string(),
            width: 8,
            driver: None,
            is_input: true,
            is_output: false,
            is_detection: false,
        };

        let pair = expander.create_dual_rail(&original);

        // Check that two signals were created
        assert_eq!(expander.lir.signals.len(), 2);
        assert_eq!(expander.lir.signals[pair.t.0 as usize].name, "data_t");
        assert_eq!(expander.lir.signals[pair.f.0 as usize].name, "data_f");
        assert_eq!(expander.lir.signals[pair.t.0 as usize].width, 8);
        assert_eq!(expander.lir.signals[pair.f.0 as usize].width, 8);
    }

    #[test]
    fn test_expand_simple_and() {
        let config = NclConfig::default();
        let mut expander = NclExpander::new("test", config);

        // Create two input signals
        let a = DualRailPair {
            t: expander.alloc_signal("a_t".to_string(), 1),
            f: expander.alloc_signal("a_f".to_string(), 1),
        };
        let b = DualRailPair {
            t: expander.alloc_signal("b_t".to_string(), 1),
            f: expander.alloc_signal("b_f".to_string(), 1),
        };
        // Create destination signals
        let dest = DualRailPair {
            t: expander.alloc_signal("out_t".to_string(), 1),
            f: expander.alloc_signal("out_f".to_string(), 1),
        };

        expander.expand_and(a, b, dest, 1);

        // Check that two nodes were created (Th22 for t, Th12 for f)
        assert_eq!(expander.lir.nodes.len(), 2);
        assert!(matches!(expander.lir.nodes[0].op, LirOp::Th22 { width: 1 }));
        assert!(matches!(expander.lir.nodes[1].op, LirOp::Th12 { width: 1 }));
    }

    #[test]
    fn test_expand_not_swaps_rails() {
        let config = NclConfig::default();
        let mut expander = NclExpander::new("test", config);

        let a = DualRailPair {
            t: expander.alloc_signal("a_t".to_string(), 1),
            f: expander.alloc_signal("a_f".to_string(), 1),
        };
        // Create destination signals
        let dest = DualRailPair {
            t: expander.alloc_signal("out_t".to_string(), 1),
            f: expander.alloc_signal("out_f".to_string(), 1),
        };

        expander.expand_not(a, dest, 1);

        // NOT should create two buffer nodes that swap the rails
        assert_eq!(expander.lir.nodes.len(), 2);
        assert!(matches!(expander.lir.nodes[0].op, LirOp::Buf { width: 1 }));
        assert!(matches!(expander.lir.nodes[1].op, LirOp::Buf { width: 1 }));
        // First buffer: a.f -> dest.t
        assert_eq!(expander.lir.nodes[0].inputs[0], a.f);
        assert_eq!(expander.lir.nodes[0].output, dest.t);
        // Second buffer: a.t -> dest.f
        assert_eq!(expander.lir.nodes[1].inputs[0], a.t);
        assert_eq!(expander.lir.nodes[1].output, dest.f);
    }

    #[test]
    fn test_completion_detection() {
        let config = NclConfig::default();
        let mut expander = NclExpander::new("test", config);

        let signals = vec![
            DualRailPair {
                t: expander.alloc_signal("sig0_t".to_string(), 1),
                f: expander.alloc_signal("sig0_f".to_string(), 1),
            },
            DualRailPair {
                t: expander.alloc_signal("sig1_t".to_string(), 1),
                f: expander.alloc_signal("sig1_f".to_string(), 1),
            },
        ];

        let completion = expander.create_completion(&signals);

        // Check that completion signal was created
        assert!(expander.lir.signals[completion.0 as usize]
            .name
            .contains("completion"));
        assert_eq!(expander.lir.signals[completion.0 as usize].width, 1);

        // Check that completion node was created
        assert_eq!(expander.lir.nodes.len(), 1);
        assert!(matches!(
            expander.lir.nodes[0].op,
            LirOp::NclComplete { width: 2 }
        ));
    }
}
