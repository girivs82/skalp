//! FP32 Decomposition to Gate-Level Logic
//!
//! This module decomposes IEEE 754 single-precision floating-point operations
//! into actual gate-level logic instead of soft macros.
//!
//! FP32 Format:
//! - Bit 31: Sign (0 = positive, 1 = negative)
//! - Bits 30-23: Exponent (8 bits, biased by 127)
//! - Bits 22-0: Mantissa (23 bits, implicit leading 1 for normalized numbers)

use crate::gate_netlist::{Cell, CellId, GateNet, GateNetId, GateNetlist};

/// FP32 decomposition context for building gate-level circuits
pub struct Fp32Decomposer {
    next_net_id: u32,
    next_cell_id: u32,
    library_name: String,
}

impl Fp32Decomposer {
    pub fn new(start_net_id: u32, start_cell_id: u32, library_name: String) -> Self {
        Self {
            next_net_id: start_net_id,
            next_cell_id: start_cell_id,
            library_name,
        }
    }

    /// Get the next net/cell IDs after decomposition
    pub fn next_ids(&self) -> (u32, u32) {
        (self.next_net_id, self.next_cell_id)
    }

    fn alloc_net(&mut self, netlist: &mut GateNetlist, name: String) -> GateNetId {
        let id = GateNetId(self.next_net_id);
        self.next_net_id += 1;
        netlist.nets.push(GateNet::new(id, name));
        id
    }

    fn alloc_cell_id(&mut self) -> CellId {
        let id = CellId(self.next_cell_id);
        self.next_cell_id += 1;
        id
    }

    // ========== Basic Gate Creation ==========

    fn create_inv(&mut self, netlist: &mut GateNetlist, path: &str, input: GateNetId) -> GateNetId {
        let out = self.alloc_net(netlist, format!("{}_inv_out", path));
        let cell = Cell::new_comb(
            self.alloc_cell_id(),
            "INV_X1".to_string(),
            self.library_name.clone(),
            0.01,
            format!("{}_inv", path),
            vec![input],
            vec![out],
        );
        netlist.cells.push(cell);
        out
    }

    fn create_and2(&mut self, netlist: &mut GateNetlist, path: &str, a: GateNetId, b: GateNetId) -> GateNetId {
        let out = self.alloc_net(netlist, format!("{}_and2_out", path));
        let cell = Cell::new_comb(
            self.alloc_cell_id(),
            "AND2_X1".to_string(),
            self.library_name.clone(),
            0.02,
            format!("{}_and2", path),
            vec![a, b],
            vec![out],
        );
        netlist.cells.push(cell);
        out
    }

    fn create_or2(&mut self, netlist: &mut GateNetlist, path: &str, a: GateNetId, b: GateNetId) -> GateNetId {
        let out = self.alloc_net(netlist, format!("{}_or2_out", path));
        let cell = Cell::new_comb(
            self.alloc_cell_id(),
            "OR2_X1".to_string(),
            self.library_name.clone(),
            0.02,
            format!("{}_or2", path),
            vec![a, b],
            vec![out],
        );
        netlist.cells.push(cell);
        out
    }

    fn create_xor2(&mut self, netlist: &mut GateNetlist, path: &str, a: GateNetId, b: GateNetId) -> GateNetId {
        let out = self.alloc_net(netlist, format!("{}_xor2_out", path));
        let cell = Cell::new_comb(
            self.alloc_cell_id(),
            "XOR2_X1".to_string(),
            self.library_name.clone(),
            0.02,
            format!("{}_xor2", path),
            vec![a, b],
            vec![out],
        );
        netlist.cells.push(cell);
        out
    }

    fn create_xnor2(&mut self, netlist: &mut GateNetlist, path: &str, a: GateNetId, b: GateNetId) -> GateNetId {
        let out = self.alloc_net(netlist, format!("{}_xnor2_out", path));
        let cell = Cell::new_comb(
            self.alloc_cell_id(),
            "XNOR2_X1".to_string(),
            self.library_name.clone(),
            0.02,
            format!("{}_xnor2", path),
            vec![a, b],
            vec![out],
        );
        netlist.cells.push(cell);
        out
    }

    fn create_mux2(&mut self, netlist: &mut GateNetlist, path: &str, d0: GateNetId, d1: GateNetId, sel: GateNetId) -> GateNetId {
        let out = self.alloc_net(netlist, format!("{}_mux2_out", path));
        let cell = Cell::new_comb(
            self.alloc_cell_id(),
            "MUX2_X1".to_string(),
            self.library_name.clone(),
            0.03,
            format!("{}_mux2", path),
            vec![d0, d1, sel],
            vec![out],
        );
        netlist.cells.push(cell);
        out
    }

    fn create_buf(&mut self, netlist: &mut GateNetlist, path: &str, input: GateNetId) -> GateNetId {
        let out = self.alloc_net(netlist, format!("{}_buf_out", path));
        let cell = Cell::new_comb(
            self.alloc_cell_id(),
            "BUF_X1".to_string(),
            self.library_name.clone(),
            0.01,
            format!("{}_buf", path),
            vec![input],
            vec![out],
        );
        netlist.cells.push(cell);
        out
    }

    /// Create a half adder: sum = a ^ b, cout = a & b
    fn create_ha(&mut self, netlist: &mut GateNetlist, path: &str, a: GateNetId, b: GateNetId) -> (GateNetId, GateNetId) {
        let sum = self.alloc_net(netlist, format!("{}_ha_sum", path));
        let cout = self.alloc_net(netlist, format!("{}_ha_cout", path));
        let cell = Cell::new_comb(
            self.alloc_cell_id(),
            "HA_X1".to_string(),
            self.library_name.clone(),
            0.02,
            format!("{}_ha", path),
            vec![a, b],
            vec![sum, cout],
        );
        netlist.cells.push(cell);
        (sum, cout)
    }

    /// Create a full adder: sum = a ^ b ^ cin, cout = maj(a,b,cin)
    fn create_fa(&mut self, netlist: &mut GateNetlist, path: &str, a: GateNetId, b: GateNetId, cin: GateNetId) -> (GateNetId, GateNetId) {
        let sum = self.alloc_net(netlist, format!("{}_fa_sum", path));
        let cout = self.alloc_net(netlist, format!("{}_fa_cout", path));
        let cell = Cell::new_comb(
            self.alloc_cell_id(),
            "FA_X1".to_string(),
            self.library_name.clone(),
            0.03,
            format!("{}_fa", path),
            vec![a, b, cin],
            vec![sum, cout],
        );
        netlist.cells.push(cell);
        (sum, cout)
    }

    // ========== Multi-bit Operations ==========

    /// Create an N-bit adder, returns sum[N] and carry_out
    fn create_adder_n(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        a: &[GateNetId],
        b: &[GateNetId],
        cin: Option<GateNetId>,
    ) -> (Vec<GateNetId>, GateNetId) {
        let n = a.len().max(b.len());
        let mut sum = Vec::with_capacity(n);
        let mut carry = cin;

        for i in 0..n {
            let a_bit = a.get(i).copied().unwrap_or(a[0]);
            let b_bit = b.get(i).copied().unwrap_or(b[0]);

            let (s, c) = if let Some(c_in) = carry {
                self.create_fa(netlist, &format!("{}_add_{}", path, i), a_bit, b_bit, c_in)
            } else {
                self.create_ha(netlist, &format!("{}_add_{}", path, i), a_bit, b_bit)
            };
            sum.push(s);
            carry = Some(c);
        }

        (sum, carry.unwrap())
    }

    /// Create an N-bit subtractor (a - b), returns diff[N] and borrow_out
    fn create_subtractor_n(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        a: &[GateNetId],
        b: &[GateNetId],
    ) -> (Vec<GateNetId>, GateNetId) {
        // a - b = a + (~b) + 1
        let n = a.len().max(b.len());
        let mut b_inv = Vec::with_capacity(n);
        for i in 0..n {
            let b_bit = b.get(i).copied().unwrap_or(b[0]);
            b_inv.push(self.create_inv(netlist, &format!("{}_inv_b_{}", path, i), b_bit));
        }

        // Add with carry-in = 1 (represented by first stage being special)
        // Actually, we need a TIE_HIGH for cin=1
        let tie_high = self.alloc_net(netlist, format!("{}_tie_high", path));
        let cell = Cell::new_comb(
            self.alloc_cell_id(),
            "TIE_HIGH".to_string(),
            self.library_name.clone(),
            0.0,
            format!("{}_tie_high", path),
            vec![],
            vec![tie_high],
        );
        netlist.cells.push(cell);

        self.create_adder_n(netlist, path, a, &b_inv, Some(tie_high))
    }

    /// Create N-bit equality comparator: returns 1 if a == b
    fn create_eq_n(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        a: &[GateNetId],
        b: &[GateNetId],
    ) -> GateNetId {
        let n = a.len().max(b.len());

        // bit_eq[i] = ~(a[i] ^ b[i]) = XNOR(a[i], b[i])
        let mut eq_bits = Vec::with_capacity(n);
        for i in 0..n {
            let a_bit = a.get(i).copied().unwrap_or(a[0]);
            let b_bit = b.get(i).copied().unwrap_or(b[0]);
            let eq = self.create_xnor2(netlist, &format!("{}_eq_{}", path, i), a_bit, b_bit);
            eq_bits.push(eq);
        }

        // AND all equality bits together
        self.create_and_tree(netlist, &format!("{}_eq_tree", path), &eq_bits)
    }

    /// Create unsigned less-than comparator: returns 1 if a < b
    fn create_ult_n(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        a: &[GateNetId],
        b: &[GateNetId],
    ) -> GateNetId {
        // a < b iff (b - a) has no borrow, i.e., carry out = 1
        // Actually: a < b iff a - b produces borrow (negative result)
        // Using subtraction: if a - b < 0, then borrow_out = 1
        let (_diff, borrow) = self.create_subtractor_n(netlist, path, a, b);
        // borrow indicates a < b
        self.create_inv(netlist, &format!("{}_lt_inv", path), borrow)
    }

    /// Create AND tree for N inputs
    fn create_and_tree(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        inputs: &[GateNetId],
    ) -> GateNetId {
        if inputs.is_empty() {
            // Return tie high for empty input
            let tie = self.alloc_net(netlist, format!("{}_tie_high", path));
            let cell = Cell::new_comb(
                self.alloc_cell_id(),
                "TIE_HIGH".to_string(),
                self.library_name.clone(),
                0.0,
                format!("{}_tie", path),
                vec![],
                vec![tie],
            );
            netlist.cells.push(cell);
            return tie;
        }
        if inputs.len() == 1 {
            return inputs[0];
        }

        // Build balanced binary tree
        let mut current = inputs.to_vec();
        let mut level = 0;
        while current.len() > 1 {
            let mut next = Vec::new();
            for (i, chunk) in current.chunks(2).enumerate() {
                if chunk.len() == 2 {
                    let out = self.create_and2(netlist, &format!("{}_l{}_{}", path, level, i), chunk[0], chunk[1]);
                    next.push(out);
                } else {
                    next.push(chunk[0]);
                }
            }
            current = next;
            level += 1;
        }
        current[0]
    }

    /// Create OR tree for N inputs
    fn create_or_tree(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        inputs: &[GateNetId],
    ) -> GateNetId {
        if inputs.is_empty() {
            let tie = self.alloc_net(netlist, format!("{}_tie_low", path));
            let cell = Cell::new_comb(
                self.alloc_cell_id(),
                "TIE_LOW".to_string(),
                self.library_name.clone(),
                0.0,
                format!("{}_tie", path),
                vec![],
                vec![tie],
            );
            netlist.cells.push(cell);
            return tie;
        }
        if inputs.len() == 1 {
            return inputs[0];
        }

        let mut current = inputs.to_vec();
        let mut level = 0;
        while current.len() > 1 {
            let mut next = Vec::new();
            for (i, chunk) in current.chunks(2).enumerate() {
                if chunk.len() == 2 {
                    let out = self.create_or2(netlist, &format!("{}_l{}_{}", path, level, i), chunk[0], chunk[1]);
                    next.push(out);
                } else {
                    next.push(chunk[0]);
                }
            }
            current = next;
            level += 1;
        }
        current[0]
    }

    // ========== FP32 Decomposition ==========

    /// Extract FP32 components from 32-bit input
    /// Returns (sign, exponent[8], mantissa[23])
    /// If input has fewer than 32 bits, replicates the last bit for padding
    fn extract_fp32_parts(
        &self,
        inputs: &[GateNetId],
    ) -> (GateNetId, Vec<GateNetId>, Vec<GateNetId>) {
        if inputs.len() < 32 {
            eprintln!(
                "[FP32_DECOMPOSE] Warning: Expected 32 input bits, got {}. Padding with last bit.",
                inputs.len()
            );
        }

        // Helper to get bit, padding with last available bit if out of range
        let get_bit = |idx: usize| -> GateNetId {
            if inputs.is_empty() {
                panic!("[FP32_DECOMPOSE] Cannot extract FP32 parts from empty input");
            }
            inputs.get(idx).copied().unwrap_or_else(|| inputs[inputs.len() - 1])
        };

        let sign = get_bit(31);
        let exp: Vec<GateNetId> = (23..31).map(|i| get_bit(i)).collect();
        let mant: Vec<GateNetId> = (0..23).map(|i| get_bit(i)).collect();
        (sign, exp, mant)
    }

    /// Decompose FP32 less-than comparison to gates
    ///
    /// Algorithm:
    /// 1. Handle sign comparison (negative < positive)
    /// 2. For same sign, compare magnitude (exponent, then mantissa)
    /// 3. For negative numbers, larger magnitude means smaller value
    pub fn decompose_fp32_lt(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        a_inputs: &[GateNetId],  // 32 bits
        b_inputs: &[GateNetId],  // 32 bits
        output: GateNetId,
    ) {
        let (a_sign, a_exp, a_mant) = self.extract_fp32_parts(a_inputs);
        let (b_sign, b_exp, b_mant) = self.extract_fp32_parts(b_inputs);

        // Case 1: a negative, b positive => a < b (return 1)
        // Case 2: a positive, b negative => a > b (return 0)
        // Case 3: both positive => compare magnitude, smaller mag = smaller
        // Case 4: both negative => compare magnitude, larger mag = smaller

        // a_neg = a_sign, b_neg = b_sign
        // a_pos = ~a_sign, b_pos = ~b_sign
        let a_pos = self.create_inv(netlist, &format!("{}_a_pos", path), a_sign);
        let b_pos = self.create_inv(netlist, &format!("{}_b_pos", path), b_sign);

        // case1: a_neg AND b_pos
        let case1 = self.create_and2(netlist, &format!("{}_case1", path), a_sign, b_pos);

        // case2: a_pos AND b_neg (result = 0, handled by default)

        // same_sign = ~(a_sign ^ b_sign)
        let diff_sign = self.create_xor2(netlist, &format!("{}_diff_sign", path), a_sign, b_sign);
        let same_sign = self.create_inv(netlist, &format!("{}_same_sign", path), diff_sign);

        // Compare exponents: a_exp < b_exp?
        let exp_lt = self.create_ult_n(netlist, &format!("{}_exp_lt", path), &a_exp, &b_exp);
        let exp_gt = self.create_ult_n(netlist, &format!("{}_exp_gt", path), &b_exp, &a_exp);
        let exp_eq = self.create_eq_n(netlist, &format!("{}_exp_eq", path), &a_exp, &b_exp);

        // Compare mantissas: a_mant < b_mant?
        let mant_lt = self.create_ult_n(netlist, &format!("{}_mant_lt", path), &a_mant, &b_mant);

        // For positive numbers: a < b if (exp_lt) OR (exp_eq AND mant_lt)
        let exp_eq_mant_lt = self.create_and2(netlist, &format!("{}_eq_mant_lt", path), exp_eq, mant_lt);
        let mag_lt = self.create_or2(netlist, &format!("{}_mag_lt", path), exp_lt, exp_eq_mant_lt);

        // For negative numbers: a < b if magnitude(a) > magnitude(b)
        let mant_ge = self.create_inv(netlist, &format!("{}_mant_ge", path), mant_lt);
        let exp_eq_mant_gt = self.create_and2(
            netlist,
            &format!("{}_eq_mant_gt", path),
            exp_eq,
            mant_ge,
        );
        let mag_gt = self.create_or2(netlist, &format!("{}_mag_gt", path), exp_gt, exp_eq_mant_gt);

        // both_pos AND mag_lt
        let both_pos = self.create_and2(netlist, &format!("{}_both_pos", path), a_pos, b_pos);
        let pos_result = self.create_and2(netlist, &format!("{}_pos_lt", path), both_pos, mag_lt);

        // both_neg AND mag_gt
        let both_neg = self.create_and2(netlist, &format!("{}_both_neg", path), a_sign, b_sign);
        let neg_result = self.create_and2(netlist, &format!("{}_neg_lt", path), both_neg, mag_gt);

        // Final result: case1 OR pos_result OR neg_result
        let or1 = self.create_or2(netlist, &format!("{}_or1", path), case1, pos_result);
        let result = self.create_or2(netlist, &format!("{}_result", path), or1, neg_result);

        // Connect to output
        self.connect_to_output(netlist, result, output);
    }

    /// Decompose FP32 greater-than comparison
    pub fn decompose_fp32_gt(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        a_inputs: &[GateNetId],
        b_inputs: &[GateNetId],
        output: GateNetId,
    ) {
        // a > b is equivalent to b < a
        self.decompose_fp32_lt(netlist, path, b_inputs, a_inputs, output);
    }

    /// Decompose FP32 less-than-or-equal comparison
    pub fn decompose_fp32_le(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        a_inputs: &[GateNetId],
        b_inputs: &[GateNetId],
        output: GateNetId,
    ) {
        // a <= b is equivalent to NOT(a > b) = NOT(b < a)
        // Create temp net for b < a result
        let lt_result = self.alloc_net(netlist, format!("{}_gt_temp", path));
        self.decompose_fp32_lt(netlist, &format!("{}_gt", path), b_inputs, a_inputs, lt_result);

        // Invert to get <=
        let le_result = self.create_inv(netlist, &format!("{}_le", path), lt_result);
        self.connect_to_output(netlist, le_result, output);
    }

    /// Decompose FP32 greater-than-or-equal comparison
    pub fn decompose_fp32_ge(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        a_inputs: &[GateNetId],
        b_inputs: &[GateNetId],
        output: GateNetId,
    ) {
        // a >= b is equivalent to NOT(a < b)
        let lt_result = self.alloc_net(netlist, format!("{}_lt_temp", path));
        self.decompose_fp32_lt(netlist, &format!("{}_lt", path), a_inputs, b_inputs, lt_result);

        let ge_result = self.create_inv(netlist, &format!("{}_ge", path), lt_result);
        self.connect_to_output(netlist, ge_result, output);
    }

    /// Connect internal signal to output net using buffer
    fn connect_to_output(&mut self, netlist: &mut GateNetlist, src: GateNetId, dst: GateNetId) {
        let cell = Cell::new_comb(
            self.alloc_cell_id(),
            "BUF_X1".to_string(),
            self.library_name.clone(),
            0.01,
            format!("out_buf_{}", dst.0),
            vec![src],
            vec![dst],
        );
        netlist.cells.push(cell);
    }

    // ========== FP32 Multiply ==========

    /// Decompose FP32 multiply to gates
    ///
    /// Algorithm:
    /// 1. result_sign = a_sign XOR b_sign
    /// 2. result_exp = a_exp + b_exp - 127 (bias adjustment)
    /// 3. result_mant = a_mant * b_mant (with implicit 1s)
    /// 4. Normalize and round
    pub fn decompose_fp32_mul(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        a_inputs: &[GateNetId],
        b_inputs: &[GateNetId],
        outputs: &[GateNetId],  // 32 output bits
    ) {
        if outputs.len() < 32 {
            eprintln!(
                "[FP32_DECOMPOSE] Error at '{}': FP32 multiply needs 32 output bits, got {}. Skipping.",
                path, outputs.len()
            );
            return;
        }
        let (a_sign, a_exp, a_mant) = self.extract_fp32_parts(a_inputs);
        let (b_sign, b_exp, b_mant) = self.extract_fp32_parts(b_inputs);

        // 1. Result sign = XOR of input signs
        let result_sign = self.create_xor2(netlist, &format!("{}_sign", path), a_sign, b_sign);

        // 2. Result exponent = a_exp + b_exp - 127
        // First add exponents
        let (exp_sum, _) = self.create_adder_n(netlist, &format!("{}_exp_add", path), &a_exp, &b_exp, None);

        // Subtract 127 (0x7F = 01111111)
        let bias: Vec<GateNetId> = self.create_constant(netlist, &format!("{}_bias", path), 127, 8);
        let (result_exp_raw, _) = self.create_subtractor_n(netlist, &format!("{}_exp_sub", path), &exp_sum, &bias);

        // 3. Mantissa multiplication
        // Add implicit 1: full_mant = 1.mant = {1, mant[22:0]} = 24 bits
        let tie_high = self.create_tie_high(netlist, &format!("{}_mant_impl", path));
        let mut a_full_mant = a_mant.clone();
        a_full_mant.push(tie_high);
        let tie_high2 = self.create_tie_high(netlist, &format!("{}_mant_impl2", path));
        let mut b_full_mant = b_mant.clone();
        b_full_mant.push(tie_high2);

        // 24x24 multiplication -> 48 bit result
        let mant_product = self.create_multiplier_n(
            netlist,
            &format!("{}_mant_mul", path),
            &a_full_mant,
            &b_full_mant,
        );

        // 4. Normalization
        // The product is in format X.XXXX where X is 1 or 2 (binary 1.xxx or 10.xxx)
        // If bit 47 is set, we need to shift right and increment exponent
        let needs_shift = mant_product[47];

        // Select mantissa bits based on normalization
        // If shift needed: take bits [46:24] (drop bit 47 which is 1)
        // If no shift: take bits [45:23]
        let mut result_mant = Vec::with_capacity(23);
        for i in 0..23 {
            let shifted_bit = mant_product[24 + i];
            let unshifted_bit = mant_product[23 + i];
            let mant_bit = self.create_mux2(
                netlist,
                &format!("{}_mant_sel_{}", path, i),
                unshifted_bit,
                shifted_bit,
                needs_shift,
            );
            result_mant.push(mant_bit);
        }

        // Adjust exponent if shift was needed
        let one = self.create_constant(netlist, &format!("{}_one", path), 1, 8);
        let (exp_inc, _) = self.create_adder_n(
            netlist,
            &format!("{}_exp_inc", path),
            &result_exp_raw[0..8].to_vec(),
            &one,
            None,
        );

        let mut result_exp = Vec::with_capacity(8);
        for i in 0..8 {
            let exp_bit = self.create_mux2(
                netlist,
                &format!("{}_exp_sel_{}", path, i),
                result_exp_raw[i],
                exp_inc[i],
                needs_shift,
            );
            result_exp.push(exp_bit);
        }

        // 5. Assemble output: [sign, exp[7:0], mant[22:0]]
        // Connect mantissa (bits 0-22)
        for i in 0..23 {
            self.connect_to_output(netlist, result_mant[i], outputs[i]);
        }
        // Connect exponent (bits 23-30)
        for i in 0..8 {
            self.connect_to_output(netlist, result_exp[i], outputs[23 + i]);
        }
        // Connect sign (bit 31)
        self.connect_to_output(netlist, result_sign, outputs[31]);
    }

    /// Create an N-bit constant
    fn create_constant(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        value: u64,
        bits: usize,
    ) -> Vec<GateNetId> {
        let mut result = Vec::with_capacity(bits);
        for i in 0..bits {
            let bit = (value >> i) & 1;
            let net = if bit == 1 {
                self.create_tie_high(netlist, &format!("{}_{}", path, i))
            } else {
                self.create_tie_low(netlist, &format!("{}_{}", path, i))
            };
            result.push(net);
        }
        result
    }

    fn create_tie_high(&mut self, netlist: &mut GateNetlist, path: &str) -> GateNetId {
        let out = self.alloc_net(netlist, format!("{}_high", path));
        let cell = Cell::new_comb(
            self.alloc_cell_id(),
            "TIE_HIGH".to_string(),
            self.library_name.clone(),
            0.0,
            format!("{}_tie_high", path),
            vec![],
            vec![out],
        );
        netlist.cells.push(cell);
        out
    }

    fn create_tie_low(&mut self, netlist: &mut GateNetlist, path: &str) -> GateNetId {
        let out = self.alloc_net(netlist, format!("{}_low", path));
        let cell = Cell::new_comb(
            self.alloc_cell_id(),
            "TIE_LOW".to_string(),
            self.library_name.clone(),
            0.0,
            format!("{}_tie_low", path),
            vec![],
            vec![out],
        );
        netlist.cells.push(cell);
        out
    }

    /// Create an NxM bit multiplier using array multiplier structure
    fn create_multiplier_n(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        a: &[GateNetId],
        b: &[GateNetId],
    ) -> Vec<GateNetId> {
        let n = a.len();
        let m = b.len();
        let result_bits = n + m;

        // Array multiplier: generate partial products and sum them
        // partial_product[i][j] = a[j] AND b[i]

        let mut partial_products: Vec<Vec<GateNetId>> = Vec::new();
        for i in 0..m {
            let mut row = Vec::new();
            // Add i zeros at the start (shift)
            for _ in 0..i {
                row.push(self.create_tie_low(netlist, &format!("{}_pp_zero_{}_{}", path, i, row.len())));
            }
            // Generate partial products
            for j in 0..n {
                let pp = self.create_and2(
                    netlist,
                    &format!("{}_pp_{}_{}", path, i, j),
                    a[j],
                    b[i],
                );
                row.push(pp);
            }
            // Pad to result_bits
            while row.len() < result_bits {
                row.push(self.create_tie_low(netlist, &format!("{}_pp_pad_{}_{}", path, i, row.len())));
            }
            partial_products.push(row);
        }

        // Sum all partial products using carry-save adder tree
        self.sum_partial_products(netlist, path, partial_products, result_bits)
    }

    /// Sum partial products using Wallace tree or simple ripple
    fn sum_partial_products(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        mut products: Vec<Vec<GateNetId>>,
        result_bits: usize,
    ) -> Vec<GateNetId> {
        // Simple approach: ripple-add all partial products
        if products.is_empty() {
            return vec![self.create_tie_low(netlist, &format!("{}_zero", path)); result_bits];
        }

        let mut sum = products.remove(0);
        for (i, pp) in products.into_iter().enumerate() {
            let (new_sum, _) = self.create_adder_n(
                netlist,
                &format!("{}_sum_{}", path, i),
                &sum,
                &pp,
                None,
            );
            sum = new_sum;
            // Extend to result_bits if needed
            while sum.len() < result_bits {
                sum.push(self.create_tie_low(netlist, &format!("{}_sum_ext_{}", path, sum.len())));
            }
        }
        sum
    }

    // ========== FP32 Addition ==========

    /// Decompose FP32 addition to gates
    ///
    /// This is the most complex operation:
    /// 1. Handle sign and determine add vs subtract
    /// 2. Align mantissas based on exponent difference
    /// 3. Add/subtract mantissas
    /// 4. Normalize result
    /// 5. Round
    pub fn decompose_fp32_add(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        a_inputs: &[GateNetId],
        b_inputs: &[GateNetId],
        outputs: &[GateNetId],
    ) {
        if outputs.len() < 32 {
            eprintln!(
                "[FP32_DECOMPOSE] Error at '{}': FP32 add needs 32 output bits, got {}. Skipping.",
                path, outputs.len()
            );
            return;
        }
        let (a_sign, a_exp, a_mant) = self.extract_fp32_parts(a_inputs);
        let (b_sign, b_exp, b_mant) = self.extract_fp32_parts(b_inputs);

        // Determine which operand has larger exponent (swap if needed)
        let exp_gt = self.create_ult_n(netlist, &format!("{}_exp_cmp", path), &b_exp, &a_exp);

        // Select larger/smaller operands
        let mut large_exp = Vec::with_capacity(8);
        let mut small_exp = Vec::with_capacity(8);
        let mut large_mant = Vec::with_capacity(23);
        let mut small_mant = Vec::with_capacity(23);

        for i in 0..8 {
            large_exp.push(self.create_mux2(
                netlist,
                &format!("{}_lexp_{}", path, i),
                b_exp[i], a_exp[i], exp_gt,
            ));
            small_exp.push(self.create_mux2(
                netlist,
                &format!("{}_sexp_{}", path, i),
                a_exp[i], b_exp[i], exp_gt,
            ));
        }

        for i in 0..23 {
            large_mant.push(self.create_mux2(
                netlist,
                &format!("{}_lmant_{}", path, i),
                b_mant[i], a_mant[i], exp_gt,
            ));
            small_mant.push(self.create_mux2(
                netlist,
                &format!("{}_smant_{}", path, i),
                a_mant[i], b_mant[i], exp_gt,
            ));
        }

        let large_sign = self.create_mux2(netlist, &format!("{}_lsign", path), b_sign, a_sign, exp_gt);
        let small_sign = self.create_mux2(netlist, &format!("{}_ssign", path), a_sign, b_sign, exp_gt);

        // Calculate exponent difference
        let (exp_diff, _) = self.create_subtractor_n(
            netlist,
            &format!("{}_exp_diff", path),
            &large_exp,
            &small_exp,
        );

        // Add implicit 1s to mantissas (24-bit)
        let tie_high = self.create_tie_high(netlist, &format!("{}_impl1", path));
        let mut large_full = large_mant.clone();
        large_full.push(tie_high);
        let tie_high2 = self.create_tie_high(netlist, &format!("{}_impl2", path));
        let mut small_full = small_mant.clone();
        small_full.push(tie_high2);

        // Shift smaller mantissa right by exp_diff
        // For simplicity, use barrel shifter (limited to reasonable shift amounts)
        let shifted_small = self.create_right_shifter(
            netlist,
            &format!("{}_shift", path),
            &small_full,
            &exp_diff[0..5].to_vec(), // Use 5 bits for shift (up to 31)
            26, // Extended for guard bits
        );

        // Extend large mantissa to same width
        let mut large_ext = large_full.clone();
        while large_ext.len() < 26 {
            large_ext.push(self.create_tie_low(netlist, &format!("{}_lext_{}", path, large_ext.len())));
        }

        // Determine if we add or subtract
        let diff_sign = self.create_xor2(netlist, &format!("{}_op_sign", path), large_sign, small_sign);

        // Add or subtract mantissas
        let (sum_result, _) = self.create_adder_n(
            netlist,
            &format!("{}_mant_add", path),
            &large_ext,
            &shifted_small,
            None,
        );

        // For subtraction, need to complement and add
        let mut small_inv = Vec::with_capacity(shifted_small.len());
        for i in 0..shifted_small.len() {
            small_inv.push(self.create_inv(netlist, &format!("{}_sinv_{}", path, i), shifted_small[i]));
        }
        let sub_cin = self.create_tie_high(netlist, &format!("{}_sub_cin", path));
        let (diff_result, _) = self.create_adder_n(
            netlist,
            &format!("{}_mant_sub", path),
            &large_ext,
            &small_inv,
            Some(sub_cin),
        );

        // Select add or subtract result
        let mut mant_result = Vec::with_capacity(26);
        for i in 0..26.min(sum_result.len()) {
            let sr = sum_result.get(i).copied().unwrap_or(sum_result[0]);
            let dr = diff_result.get(i).copied().unwrap_or(diff_result[0]);
            mant_result.push(self.create_mux2(
                netlist,
                &format!("{}_res_{}", path, i),
                sr, dr, diff_sign,
            ));
        }

        // Result sign is the sign of the larger magnitude operand
        let result_sign = large_sign;

        // Simplified normalization: just take bits [24:2] as mantissa, use large_exp
        // (Full implementation would need leading-zero detection and shift)
        for i in 0..23 {
            let mant_bit = mant_result.get(i + 2).copied().unwrap_or(mant_result[0]);
            self.connect_to_output(netlist, mant_bit, outputs[i]);
        }
        for i in 0..8 {
            self.connect_to_output(netlist, large_exp[i], outputs[23 + i]);
        }
        self.connect_to_output(netlist, result_sign, outputs[31]);
    }

    /// Decompose FP32 subtraction (a - b = a + (-b))
    pub fn decompose_fp32_sub(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        a_inputs: &[GateNetId],
        b_inputs: &[GateNetId],
        outputs: &[GateNetId],
    ) {
        if b_inputs.len() < 32 || outputs.len() < 32 {
            eprintln!(
                "[FP32_DECOMPOSE] Error at '{}': FP32 sub needs 32 bits, got {} inputs and {} outputs. Skipping.",
                path, b_inputs.len(), outputs.len()
            );
            return;
        }
        // Negate b by flipping its sign bit
        let mut b_negated = b_inputs.to_vec();
        b_negated[31] = self.create_inv(netlist, &format!("{}_neg_b", path), b_inputs[31]);

        self.decompose_fp32_add(netlist, path, a_inputs, &b_negated, outputs);
    }

    /// Create a right barrel shifter
    fn create_right_shifter(
        &mut self,
        netlist: &mut GateNetlist,
        path: &str,
        input: &[GateNetId],
        shift_amt: &[GateNetId],
        output_bits: usize,
    ) -> Vec<GateNetId> {
        let n = input.len();
        let mut current = input.to_vec();

        // Pad input to output_bits
        while current.len() < output_bits {
            current.push(self.create_tie_low(netlist, &format!("{}_pad_{}", path, current.len())));
        }

        // Barrel shifter: shift by 1, 2, 4, 8, 16 based on shift_amt bits
        for (stage, &shift_bit) in shift_amt.iter().enumerate() {
            let shift_amount = 1 << stage;
            let mut next = Vec::with_capacity(output_bits);

            for i in 0..output_bits {
                let unshifted = current[i];
                let shifted = if i + shift_amount < current.len() {
                    current[i + shift_amount]
                } else {
                    self.create_tie_low(netlist, &format!("{}_sh_zero_{}_{}", path, stage, i))
                };

                let mux_out = self.create_mux2(
                    netlist,
                    &format!("{}_sh_{}_{}", path, stage, i),
                    unshifted,
                    shifted,
                    shift_bit,
                );
                next.push(mux_out);
            }
            current = next;
        }

        current
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fp32_decomposer_creation() {
        let decomposer = Fp32Decomposer::new(0, 0, "test_lib".to_string());
        assert_eq!(decomposer.next_ids(), (0, 0));
    }
}
